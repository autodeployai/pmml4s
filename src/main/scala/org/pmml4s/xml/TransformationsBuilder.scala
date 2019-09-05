/*
 * Copyright (c) 2017-2019 AutoDeploy AI
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.pmml4s.xml

import org.pmml4s.UnsupportedException
import org.pmml4s.common._
import org.pmml4s.metadata._
import org.pmml4s.transformations._
import org.pmml4s.util.Utils
import org.pmml4s.xml.XmlImplicits._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.MetaData
import scala.xml.parsing.XhtmlEntities
import scala.xml.pull._

/**
 * Builder of transformations
 */
trait TransformationsBuilder extends CommonBuilder
  with FieldScope
  with HasFieldScope
  with FunctionProvider {

  // This collection is only used when parsing derived fields within TransformationDictionary
  protected val provider: MutableFunctionProvider = new MutableFunctionProvider

  protected val derivedFieldScope: MutableFieldScope[DerivedField] = new MutableFieldScope

  override def field(name: String): Field = {
    val fld = super.field(name)
    if (!fld.referenced) {
      fld.referenced = true
    }
    fld
  }

  override def getField(name: String): Option[Field] = derivedFieldScope.getField(name)

  override def scope: FieldScope = this

  override def getFunction(name: String): Option[Function] = {
    BuiltInFunctions.getFunction(name).orElse(provider.getFunction(name))
  }

  /** Parses the transformation dictionary. */
  def makeTransformationDictionary(reader: XMLEventReader): TransformationDictionary = {
    val defineFunctions = mutable.ArrayBuilder.make[DefineFunction]()
    val derivedFields = mutable.ArrayBuilder.make[DerivedField]()
    traverseElems(reader, ElemTags.TRANSFORMATION_DICTIONARY, {
      case EvElemStart(_, ElemTags.DEFINE_FUNCTION, attrs, _) => {
        defineFunctions += (provider += makeDefineFunction(reader, attrs))
      }
      case EvElemStart(_, ElemTags.DERIVED_FIELD, attrs, _)   => {
        derivedFields += (derivedFieldScope += makeDerivedField(reader, attrs))
      }
    })

    new TransformationDictionary(derivedFields.result(), defineFunctions.result())
  }

  /** Parses the local transformations */
  def makeLocalTransformations(reader: XMLEventReader): LocalTransformations = {
    val derivedFields = makeElems(reader, ElemTags.LOCAL_TRANSFORMATIONS, ElemTags.DERIVED_FIELD, new ElemBuilder[DerivedField] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): DerivedField = {
        derivedFieldScope += makeDerivedField(reader, attrs)
      }
    })

    new LocalTransformations(derivedFields)
  }

  def makeDefineFunction(reader: XMLEventReader, attrs: XmlAttrs): DefineFunction = {
    val name = attrs(AttrTags.NAME)
    val opType = attrs.get(AttrTags.OPTYPE).map(OpType.withName(_)).getOrElse(OpType.typeless)
    val dataType = attrs.get(AttrTags.DATA_TYPE).map(DataType.withName(_)).getOrElse(UnresolvedDataType)
    val parameterFields = mutable.ArrayBuilder.make[ParameterField]
    var expr: Expression = null
    traverseElems(reader, ElemTags.DEFINE_FUNCTION, {
      case EvElemStart(_, ElemTags.PARAMETER_FIELD, attrs, _)     => parameterFields += makeElem(reader, attrs,
        new ElemBuilder[ParameterField] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): ParameterField = {
            val name = attrs(AttrTags.NAME)
            val opType = attrs.get(AttrTags.OPTYPE).map(OpType.withName(_)).getOrElse(OpType.typeless)
            val dataType = attrs.get(AttrTags.DATA_TYPE).map(DataType.withName(_)).getOrElse(UnresolvedDataType)
            val displayName = attrs.get(AttrTags.DISPLAY_NAME)

            new ParameterField(name, opType, dataType, displayName)
          }
        })
      case event: EvElemStart if Expression.contains(event.label) => {
        expr = makeExpression(reader, event, new FieldScope {
          private lazy val nameToField = parameterFields.result().map(x => (x.name, x)).toMap

          override def getField(name: String): Option[Field] = nameToField.get(name)
        })
      }
    })

    new DefineFunction(name, parameterFields.result(), expr, opType, dataType)
  }

  def makeDerivedField(reader: XMLEventReader, attrs: XmlAttrs): DerivedField = {
    val name = attrs.getString(AttrTags.NAME, "")
    val displayName = attrs.get(AttrTags.DISPLAY_NAME)
    val opType = OpType.withName(attrs(AttrTags.OPTYPE))
    val dataType = attrs.get(AttrTags.DATA_TYPE).map { x => DataType.withName(x) } getOrElse (UnresolvedDataType)
    val values = new ArrayBuffer[Value]()
    var expression: Expression = null

    traverseElems(reader, ElemTags.DERIVED_FIELD, {
      case event: EvElemStart if Expression.contains(event.label) => expression = makeExpression(reader, event, this)
      case EvElemStart(_, ElemTags.VALUE, attrs, _)               => values :+ makeValue(reader, attrs)
      case _                                                      =>
    })

    if (values.isEmpty && expression.categories.nonEmpty) {
      values ++= expression.categories.map(x => new Value(x.toString))
    }

    new DerivedField(name, displayName, dataType, opType, values, expression)
  }

  def makeExpression(reader: XMLEventReader, event: EvElemStart, scope: FieldScope): Expression = makeElem(reader, event, new GroupElemBuilder[Expression] {
    override def build(reader: XMLEventReader, event: EvElemStart): Expression = event match {
      case EvElemStart(_, ElemTags.CONSTANT, attrs, _)        => makeElem(reader, attrs, new ElemBuilder[Constant] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Constant = {
          val dataType = attrs.get(AttrTags.DATA_TYPE).map(DataType.withName(_))
          val missing = attrs.getBoolean(AttrTags.MISSING, false)
          var content: String = null
          traverseElems(reader, ElemTags.CONSTANT, {
            case EvText(text) => content = text
          }, true)

          val value = if (dataType.isDefined) {
            if (content != null && dataType.get == RealType) {
              content.toLowerCase match {
                case "nan"  => Double.NaN
                case "inf"  => Double.PositiveInfinity
                case "-inf" => Double.NegativeInfinity
                case _      => Utils.toVal(content, dataType.get)
              }
            } else Utils.toVal(content, dataType.get)
          } else content

          new Constant(value, dataType, missing)
        }
      })
      case EvElemStart(_, ElemTags.FIELD_REF, attrs, _)       => makeFieldRef(reader, attrs, scope)
      case EvElemStart(_, ElemTags.NORM_CONTINUOUS, attrs, _) => makeElem(reader, attrs, new ElemBuilder[NormContinuous] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): NormContinuous = {
          val fld = scope.field(attrs(AttrTags.FIELD))
          val mapMissingTo = attrs.getDouble(AttrTags.MAP_MISSING_TO)
          val outliers = attrs.get(AttrTags.OUTLIERS).map(OutlierTreatmentMethod.withName(_)).getOrElse(OutlierTreatmentMethod.asIs)
          val linearNorms = makeElems(reader, ElemTags.NORM_CONTINUOUS, ElemTags.LINEAR_NORM, new ElemBuilder[LinearNorm] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): LinearNorm = {
              val orig = attrs.double(AttrTags.ORIG)
              val norm = attrs.double(AttrTags.NORM)
              new LinearNorm(orig, norm)
            }
          })

          new NormContinuous(linearNorms, fld, mapMissingTo, outliers)
        }
      })
      case EvElemStart(_, ElemTags.NORM_DISCRETE, attrs, _)   => makeElem(reader, attrs, new ElemBuilder[NormDiscrete] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): NormDiscrete = {
          val fld = scope.field(attrs(AttrTags.FIELD))
          val value = fld.toVal(attrs(AttrTags.VALUE))
          val mapMissingTo = attrs.getDouble(AttrTags.MAP_MISSING_TO)

          new NormDiscrete(fld, value, mapMissingTo)
        }
      })
      case EvElemStart(_, ElemTags.DISCRETIZE, attrs, _)      => makeElem(reader, attrs, new ElemBuilder[Discretize] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Discretize = {
          val fld = scope.field(attrs(AttrTags.FIELD))
          val dataType = attrs.get(AttrTags.DATA_TYPE).map(DataType.withName(_))
          val mapMissingTo = attrs.get(AttrTags.MAP_MISSING_TO).flatMap(x => dataType.map(_.toVal(x)))
          val defaultValue = attrs.get(AttrTags.DEFAULT_VALUE).flatMap(x => dataType.map(_.toVal(x)))
          val discretizeBins = makeElems(reader, ElemTags.DISCRETIZE, ElemTags.DISCRETIZE_BIN, new ElemBuilder[DiscretizeBin] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): DiscretizeBin = {
              val binValue = attrs(AttrTags.BIN_VALUE)
              val interval = makeElem(reader, ElemTags.DISCRETIZE_BIN, ElemTags.INTERVAL, new ElemBuilder[Interval] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): Interval = makeInterval(reader, attrs)
              })

              new DiscretizeBin(interval.get, binValue)
            }
          })

          new Discretize(discretizeBins, fld, mapMissingTo, defaultValue, dataType)
        }
      })
      case EvElemStart(_, ElemTags.MAP_VALUES, attrs, _)      => makeElem(reader, attrs, new ElemBuilder[MapValues] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): MapValues = {
          val outputColumn = attrs(AttrTags.OUTPUT_COLUMN)
          val dataType = attrs.get(AttrTags.DATA_TYPE).map(DataType.withName(_))
          val mapMissingTo = attrs.get(AttrTags.MAP_MISSING_TO)
          val defaultValue = attrs.get(AttrTags.DEFAULT_VALUE)
          val fieldColumnPairs = mutable.ArrayBuilder.make[FieldColumnPair]()
          var table: Table = null
          val dataTypes = mutable.Map.empty[String, DataType]
          dataType.foreach(x => dataTypes += outputColumn -> x)

          traverseElems(reader, ElemTags.MAP_VALUES, {
            case EvElemStart(_, ElemTags.FIELD_COLUMN_PAIR, attrs, _) => fieldColumnPairs +=
              makeElem(reader, attrs, new ElemBuilder[FieldColumnPair] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): FieldColumnPair = {
                  val f = scope.field(attrs(AttrTags.FIELD))
                  val column = attrs(AttrTags.COLUMN)

                  dataTypes += column -> f.dataType
                  new FieldColumnPair(f, column)
                }
              })
            case event: EvElemStart if (Table.contains(event.label))  => table = makeTable(reader, event, dataTypes.toMap)
          })

          new MapValues(fieldColumnPairs.result(), table, outputColumn, mapMissingTo, defaultValue, dataType)
        }
      })
      case EvElemStart(_, ElemTags.TEXT_INDEX, attrs, _)      => makeElem(reader, attrs, new ElemBuilder[TextIndex] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): TextIndex = {
          val textField = scope.field(attrs(AttrTags.TEXT_FIELD))
          val localTermWeights = attrs.get(AttrTags.LOCAL_TERM_WEIGHTS).map(LocalTermWeights.withName(_)).
            getOrElse(LocalTermWeights.termFrequency)
          val isCaseSensitive = attrs.getBoolean(AttrTags.IS_CASE_SENSITIVE, false)
          val maxLevenshteinDistance = attrs.getInt(AttrTags.MAX_LEVENSHTEIN_DISTANCE, 0)
          val countHits = attrs.get(AttrTags.COUNT_HITS).map(CountHits.withName(_)).
            getOrElse(CountHits.allHits)
          val wordSeparatorCharacterRE = attrs.getString(AttrTags.WORD_SEPARATOR_CHARACTER_RE, "\\s+")
          val tokenize = attrs.getBoolean(AttrTags.TOKENIZE, true)
          val textIndexNormalizations = mutable.ArrayBuilder.make[TextIndexNormalization]()
          var expression: Expression = null

          traverseElems(reader, ElemTags.TEXT_INDEX, {
            case EvElemStart(_, ElemTags.TEXT_INDEX_NORMALIZATION, attrs, _) => textIndexNormalizations +=
              makeElem(reader, attrs, new ElemBuilder[TextIndexNormalization] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): TextIndexNormalization = {
                  val inField = attrs.getString(AttrTags.IN_FIELD, "string")
                  val outField = attrs.getString(AttrTags.OUT_FIELD, "stem")
                  val regexField = attrs.getString(AttrTags.REGEX_FIELD, "regex")
                  val recursive = attrs.getBoolean(AttrTags.RECURSIVE, false)
                  val isCaseSensitive = attrs.getBoolean(AttrTags.IS_CASE_SENSITIVE)
                  val maxLevenshteinDistance = attrs.getInt(AttrTags.MAX_LEVENSHTEIN_DISTANCE)
                  val wordSeparatorCharacterRE = attrs.get(AttrTags.WORD_SEPARATOR_CHARACTER_RE)
                  val tokenize = attrs.getBoolean(AttrTags.TOKENIZE)
                  var table: Table = null

                  traverseElems(reader, ElemTags.TEXT_INDEX_NORMALIZATION, {
                    case event: EvElemStart if (Table.contains(event.label)) => table = makeTable(reader, event)
                    case _                                                   =>
                  })
                  new TextIndexNormalization(table, isCaseSensitive, maxLevenshteinDistance, wordSeparatorCharacterRE,
                    tokenize, inField, outField, regexField, recursive)
                }
              })
            case event: EvElemStart if Expression.contains(event.label)      => expression = makeExpression(reader, event, scope)
            case _                                                           =>
          })

          new TextIndex(textField, expression, textIndexNormalizations.result(), localTermWeights, isCaseSensitive,
            maxLevenshteinDistance, countHits, wordSeparatorCharacterRE, tokenize)
        }
      })
      case EvElemStart(_, ElemTags.APPLY, attrs, _)           => makeElem(reader, attrs, new ElemBuilder[Apply] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Apply = {
          val func = function(attrs(AttrTags.FUNCTION))
          val mapMissingTo = attrs.get(AttrTags.MAP_MISSING_TO)
          val defaultValue = attrs.get(AttrTags.DEFAULT_VALUE)
          val invalidValueTreatment = attrs.get(AttrTags.INVALID_VALUE_TREATMENT).map(InvalidValueTreatment.withName(_)).
            getOrElse(InvalidValueTreatment.returnInvalid)

          val children = mutable.ArrayBuilder.make[Expression]()
          traverseElems(reader, ElemTags.APPLY, {
            case event: EvElemStart if Expression.contains(event.label) => children += makeExpression(reader, event, scope)
            case _                                                      =>
          })

          new Apply(func, children.result(), mapMissingTo, defaultValue, invalidValueTreatment)
        }
      })
      case EvElemStart(_, ElemTags.AGGREGATE, attrs, _)       => throw new UnsupportedException(ElemTags.AGGREGATE)
      case EvElemStart(_, ElemTags.LAG, attrs, _)             => throw new UnsupportedException(ElemTags.LAG)
      case _                                                  => ???
    }
  })

  def makeFieldRef(reader: XMLEventReader, attrs: MetaData, scope: FieldScope): FieldRef = makeElem(reader, attrs, new ElemBuilder[FieldRef] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): FieldRef = {
      val fld = scope.field(attrs(AttrTags.FIELD))
      val mapMissingTo = attrs.get(AttrTags.MAP_MISSING_TO).map(fld.toVal(_))

      new FieldRef(fld, mapMissingTo)
    }
  })

  def makeCategoricalPredictor(reader: XMLEventReader, attrs: MetaData): CategoricalPredictor = makeElem(reader, attrs, new ElemBuilder[CategoricalPredictor] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): CategoricalPredictor = {
      val name = attrs(AttrTags.NAME)
      val fld = field(name)
      val value = verifyValue(attrs(AttrTags.VALUE), field(name))
      val coefficient = attrs.double(AttrTags.COEFFICIENT)

      new CategoricalPredictor(fld, coefficient, value)
    }
  })

  def makeTable(reader: XMLEventReader, event: EvElemStart, dataTypes: Map[String, DataType] = Map.empty): Table = makeElem(reader, event, new GroupElemBuilder[Table] {
    override def build(reader: XMLEventReader, event: EvElemStart): Table = event match {
      case EvElemStart(_, ElemTags.TABLE_LOCATOR, attrs, _) => makeElem(reader, attrs, new ElemBuilder[TableLocator] {
        // TODO: table location
        override def build(reader: XMLEventReader, attrs: XmlAttrs): TableLocator = {
          throw new UnsupportedException(ElemTags.TABLE_LOCATOR)
        }
      })
      case EvElemStart(_, ElemTags.INLINE_TABLE, attrs, _)  => makeElem(reader, attrs, new ElemBuilder[InlineTable] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): InlineTable = {
          val rows = makeElems(reader, ElemTags.INLINE_TABLE, ElemTags.ROW, new ElemBuilder[Row] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): Row = {
              val elements = new mutable.HashMap[String, Any]()
              var key: String = null
              var value: String = ""
              traverseElems(reader, ElemTags.ROW, {
                case EvElemStart(pre, label, _, _) => {
                  // Handel some elements like <data:input>, <data:output>
                  key = if (pre != null) s"${pre}:${label}" else label
                  value = ""
                }
                case EvElemEnd(_, _)               => if (key != null) {
                  elements += {
                    key -> dataTypes.getOrElse(key, StringType).toVal(value)
                  }
                  key = null
                  value = ""
                }
                case EvEntityRef(text)             => if (key != null) {
                  value = value + XhtmlEntities.entMap.get(text).getOrElse("")
                }
                case EvText(text)                  => if (key != null) value = value + text
              }, true, true, true)

              new Row(elements.toMap)
            }
          })

          new InlineTable(rows)
        }
      })
      case _                                                => ??????
    }
  })
}

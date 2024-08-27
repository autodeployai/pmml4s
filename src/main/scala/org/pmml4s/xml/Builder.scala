/*
 * Copyright (c) 2017-2023 AutoDeployAI
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

import org.pmml4s._
import org.pmml4s.common._
import org.pmml4s.metadata._
import org.pmml4s.model.Model
import org.pmml4s.transformations.{BuiltInFunctions, Expression, LocalTransformations}
import org.pmml4s.util.{ArrayUtils, Utils}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Base class of model builder
 */
trait Builder[T <: Model] extends TransformationsBuilder {
  protected var parent: Model = _
  protected val extensions = new ArrayBuffer[Extension]
  protected var miningSchema: MiningSchema = _
  protected var output: Option[Output] = None
  protected var targets: Option[Targets] = None
  protected var localTransformations: Option[LocalTransformations] = None
  protected var modelStats: Option[ModelStats] = None
  protected var modelExplanation: Option[ModelExplanation] = None
  protected var modelVerification: Option[ModelVerification] = None

  protected lazy val wrappedFields: ArrayBuffer[WrappedField] = ArrayBuffer.empty

  protected def attributes: ModelAttributes

  protected def defaultFieldScope: FieldScope = this

  /**
   * OutputField with feature="transformedValue" in a PMML model can refer to some field after it
   */
  protected val outputFieldScope: MutableFieldScope[OutputField] = new MutableFieldScope[OutputField] {
    override def getField(name: String): Option[Field] = {
      super.getField(name) orElse {
        val wf = new WrappedField(name)
        wrappedFields += wf
        Some(wf)
      }
    }
  }

  /** Builds a PMML model from a specified XML reader. */
  def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): T

  /** Name of the builder. */
  def name: String

  def postBuild(): Unit = {
    wrappedFields.dropWhile(x => {
      val f = getField(x.name)
      if (f.isDefined) {
        x.field = f.get
        true
      } else false
    })
  }

  def traverseModel(reader: XMLEventReader, parent: String, f: PartialFunction[XMLEvent, Any]): Any = {
    var done = false
    while (!done && reader.hasNext) {
      reader.next() match {
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _)          => extHandler(reader, attrs).foreach { x =>
          extensions += x
        }
        case EvElemStart(_, ElemTags.MINING_SCHEMA, _, _)          => miningSchema = makeMiningSchema(reader)
        case EvElemStart(_, ElemTags.OUTPUT, _, _)                 => output = makeOutput(reader)
        case EvElemStart(_, ElemTags.MODEL_STATS, _, _)            => modelStats = makeModelStats(reader)
        case EvElemStart(_, ElemTags.MODEL_EXPLANATION, _, _)      => modelExplanation = makeModelExplanation(reader)
        case EvElemStart(_, ElemTags.TARGETS, _, _)                => targets = makeTargets(reader)
        case EvElemStart(_, ElemTags.LOCAL_TRANSFORMATIONS, _, _)  =>
          localTransformations = Option(makeLocalTransformations(reader))
        case EvElemStart(_, ElemTags.MODEL_VERIFICATION, attrs, _) =>
          modelVerification = makeModelVerification(reader, attrs)
        case event: EvElemStart                                    => f(event)
        case EvElemEnd(_, `parent`)                                => done = true
        case _                                                     =>
      }
    }
  }

  override def getField(name: String): Option[Field] = {
    val result: Option[Field] = if (parent != null) parent.getField(name) else None
    result orElse localTransformations.flatMap(_.get(name)) orElse derivedFieldScope.getField(name)
  }

  def target: Field = {
    val name = targets.map(_.targetName).getOrElse(miningSchema.targetName)
    if (name != null) field(name) else null
  }

  def getTarget: Option[Field] = {
    val name = targets.map(_.targetName).getOrElse(miningSchema.targetName)
    if (name != null) getField(name) else None
  }

  def verifyScore(s: String): Any = {
    if (attributes.isClassification) {
      // Check its parent target that may be different from anonymous target of the child model.
      val t = getTarget
      t.map(x => verifyValue(s, x)).getOrElse({
        // Check the parent target carefully
        var result: Any = s
        if (parent != null && parent.targetField != null && parent.targetField.isCategorical) {
          val transformedValue = parent.targetField.toVal(s)
          if (parent.targetField.isValidValue(transformedValue)) {
            result = transformedValue
          }
        }

        result
      })
    } else if (attributes.isRegression) {
      toVal(s, RealType)
    } else s
  }

  override def getFunction(name: String): Option[transformations.Function] = {
    BuiltInFunctions.getFunction(name) orElse parent.transformationDictionary.flatMap(_.getFunction(name)) orElse
      super.getFunction(name)
  }

  /** Extracts these common attributes from a model */
  protected def makeAttributes(attrs: XmlAttrs): ModelAttributes = new ModelAttributes(
    MiningFunction.withName(attrs(AttrTags.FUNCTION_NAME)),
    attrs.get(AttrTags.MODEL_NAME),
    attrs.get(AttrTags.ALGORITHM_NAME),
    attrs.getBoolean(AttrTags.IS_SCORABLE, true))

  /** Parses the mining fields under MiningSchema */
  protected def makeMiningSchema(reader: XMLEventReader): MiningSchema = {
    val miningFields = makeElems(reader, ElemTags.MINING_SCHEMA, ElemTags.MINING_FIELD, new ElemBuilder[MiningField] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): MiningField = {
        val name = attrs(AttrTags.NAME)
        val f = field(name)

        val usageType = attrs.get(AttrTags.USAGE_TYPE).map { x => UsageType.withName(x) } getOrElse UsageType.active
        val opType = attrs.get(AttrTags.OPTYPE).map { x => OpType.withName(x) }
        val importance = attrs.getDouble(AttrTags.IMPORTANCE)
        val outliers = attrs.get(AttrTags.OUTLIERS).map { x => OutlierTreatmentMethod.withName(x) } getOrElse
          OutlierTreatmentMethod.asIs
        val (lowValue, highValue) = attrs.getDouble(AttrTags.LOW_VALUE, AttrTags.HIGH_VALUE)
        val missingValueReplacement = attrs.get(AttrTags.MISSING_VALUE_REPLACEMENT).flatMap { x => f.toValOption(x) }
        val missingValueTreatment = attrs.get(AttrTags.MISSING_VALUE_TREATMENT).map { x =>
          MissingValueTreatment
            .withName(x)
        }
        val invalidValueTreatment = attrs.get(AttrTags.INVALID_VALUE_TREATMENT).map { x =>
          InvalidValueTreatment
            .withName(x)
        } getOrElse {
          if (parent != null && parent.miningSchema != null && parent.miningSchema.contains(name))
            parent.miningSchema(name).invalidValueTreatment else InvalidValueTreatment.returnInvalid
        }
        val invalidValueReplacement = attrs.get(AttrTags.INVALID_VALUE_REPLACEMENT).flatMap { x => f.toValOption(x) } orElse {
          if (parent != null && parent.miningSchema != null && parent.miningSchema.contains(name)) {
            parent.miningSchema(name).invalidValueReplacement
          } else None
        }
        new MiningField(name, usageType, opType, importance, outliers, lowValue, highValue, missingValueReplacement,
          missingValueTreatment, invalidValueTreatment, invalidValueReplacement)
      }
    })

    new MiningSchema(miningFields)
  }

  import ResultFeature._

  def inferDataType(feature: ResultFeature, targetField: Option[String]): DataType = feature match {
    case `predictedValue`                                                                                   => {
      val t = targetField.map(field(_)).orElse(getTarget)
      t.map(x => x.dataType match {
        case IntegerType => RealType
        case _           => x.dataType
      }).getOrElse(StringType)
    }
    case `probability` | `affinity` | `standardError` | `clusterAffinity` | `entityAffinity` | `confidence` => RealType
    case `predictedDisplayValue` | `clusterId` | `entityId`                                                 =>
      StringType
    case _                                                                                                  =>
      UnresolvedDataType
  }

  /** Parses the output fields */
  protected def makeOutput(reader: XMLEventReader): Option[Output] = {
    val outputFields = makeElems(reader, ElemTags.OUTPUT, ElemTags.OUTPUT_FIELD, new ElemBuilder[OutputField] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): OutputField = {
        val name = attrs(AttrTags.NAME)
        val displayName = attrs.get(AttrTags.DISPLAY_NAME)
        val opType = attrs.get(AttrTags.OPTYPE).map(OpType.withName(_)).getOrElse(OpType.typeless)
        val feature = attrs.get(AttrTags.FEATURE).map(ResultFeature.withName(_)).getOrElse(ResultFeature.predictedValue)
        val targetField = attrs.get(AttrTags.TARGET_FIELD)
        val isFinalResult = attrs.getBoolean(AttrTags.IS_FINAL_RESULT, true)
        val value = attrs.get(AttrTags.VALUE).map(x => if (feature == ResultFeature.probability) {
          var t = targetField.flatMap(getField(_)).getOrElse(target)
          if (t == null && parent != null && parent.targetField != null && parent.targetField.isCategorical) {
            t = parent.targetField
          }
          if (t != null) t.toVal(x) else x
        } else x)
        val ruleFeature = attrs.get(AttrTags.RULE_FEATURE) map { x => RuleFeature.withName(x) } getOrElse RuleFeature
          .consequent
        val algorithm = attrs.get(AttrTags.ALGORITHM) map { x => Algorithm.withName(x) } getOrElse Algorithm
          .exclusiveRecommendation
        val rank = attrs.getInt(AttrTags.RANK, 1)
        val rankBasis = attrs.get(AttrTags.RANK_BASIS) map { x => RankBasis.withName(x) } getOrElse RankBasis.confidence
        val rankOrder = attrs.get(AttrTags.RANK_ORDER) map { x => RankOrder.withName(x) } getOrElse RankOrder.descending
        val isMultiValued = Utils.toBoolean(attrs.getInt(AttrTags.IS_MULTI_VALUED, 0))
        val segmentId = attrs.get(AttrTags.SEGMENT_ID)

        // dataType is required from 4.3
        val dataType = attrs.get(AttrTags.DATA_TYPE) map { x => DataType.withName(x) } getOrElse
          inferDataType(feature, targetField)

        var expr: Expression = null
        var decisions: Decisions = null

        traverseElems(reader, ElemTags.OUTPUT_FIELD, {
          case EvElemStart(_, ElemTags.DECISIONS, attrs, _)       => decisions = makeElem(reader, attrs, new
              ElemBuilder[Decisions] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): Decisions = {
              val (businessProblem, description) = attrs.get(AttrTags.BUSINESS_PROBLEM, AttrTags.DESCRIPTION)
              val decisions = makeElems(reader, ElemTags.DECISIONS, ElemTags.DECISION, new ElemBuilder[Decision] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): Decision = {
                  val value = attrs(AttrTags.VALUE)
                  val (displayValue, description) = attrs.get(AttrTags.DISPLAY_VALUE, AttrTags.DESCRIPTION)

                  new Decision(value, displayValue, description)
                }
              })

              new Decisions(decisions, businessProblem, description)
            }
          })
          case ev: EvElemStart if (Expression.contains(ev.label)) => expr = makeExpression(reader, ev, outputFieldScope)
          case _                                                  =>
        })

        outputFieldScope += new OutputField(name, displayName, dataType, opType, feature, targetField, value,
          ruleFeature, algorithm, rank,
          rankBasis, rankOrder, isMultiValued, segmentId, isFinalResult, Option(decisions), Option(expr))
      }
    })

    Some(new Output(outputFields))
  }

  /** Parses the targets */
  protected def makeTargets(reader: XMLEventReader): Option[Targets] = {
    val targets = makeElems(reader, ElemTags.TARGETS, ElemTags.TARGET, new ElemBuilder[Target] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): Target = {
        val field = attrs.get(AttrTags.FIELD)
        val optype = attrs.get(AttrTags.OPTYPE) map { x => OpType.withName(x) }
        val castInteger = attrs.get(AttrTags.CAST_INTEGER) map { x => CastInteger.withName(x) }
        val (min, max) = attrs.getDouble(AttrTags.MIN, AttrTags.MAX)
        val rescaleConstant = attrs.getDouble(AttrTags.RESCALE_CONSTANT, 0.0)
        val rescaleFactor = attrs.getDouble(AttrTags.RESCALE_FACTOR, 1.0)

        val targetValues = makeElems(reader, ElemTags.TARGET, ElemTags.TARGET_VALUE, new ElemBuilder[TargetValue] {
          def build(reader: XMLEventReader, attrs: XmlAttrs): TargetValue = {
            val value = attrs.get(AttrTags.VALUE)
            val displayValue = attrs.get(AttrTags.DISPLAY_VALUE)
            val priorProbability = attrs.getDouble(AttrTags.PRIOR_PROBABILITY)
            val defaultValue = attrs.getDouble(AttrTags.DEFAULT_VALUE)

            new TargetValue(value, displayValue, priorProbability, defaultValue)
          }
        })

        new Target(field, optype, castInteger, min, max, rescaleConstant, rescaleFactor, targetValues)
      }
    })

    Some(new Targets(targets))
  }

  protected def makeScoreDistribution(reader: XMLEventReader, attrs: XmlAttrs): ScoreDistribution =
    makeElem(reader, attrs, new ElemBuilder[ScoreDistribution] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): ScoreDistribution = {
        val value = verifyScore(attrs(AttrTags.VALUE))
        val recordCount = attrs.double(AttrTags.RECORD_COUNT)
        val confidence = attrs.getDouble(AttrTags.CONFIDENCE)
        val probability = attrs.getDouble(AttrTags.PROBABILITY)

        new ScoreDistribution(value, recordCount, confidence, probability)
      }
    })

  /** Parses one of predicates: SimplePredicate, CompoundPredicate, SimpleSetPredicate, True, or False */
  protected def makePredicate(reader: XMLEventReader, event: EvElemStart): Predicate =
    makeElem(reader, event, new GroupElemBuilder[Predicate] {
      def build(reader: XMLEventReader, event: EvElemStart): Predicate = event match {
        case EvElemStart(_, ElemTags.SIMPLE_PREDICATE, attrs, _)     => makeElem(reader, attrs, new
            ElemBuilder[SimplePredicate] {
          def build(reader: XMLEventReader, attrs: XmlAttrs): SimplePredicate = {
            val f = field(attrs(AttrTags.FIELD))
            val operator = Operator.withName(attrs(AttrTags.OPERATOR))
            val value = if (operator != Operator.isMissing && operator != Operator.isNotMissing) {
              // Besides of the continuous values, the real numeric values of categorical fields are treated as
              // normal values, not need to encode them
              if (f.isContinuous || f.isReal) {
                // Not validate the value
                Utils.toDouble(f.toVal(attrs(AttrTags.VALUE)))
              } else {
                f.encode(f.toVal(attrs(AttrTags.VALUE)))
              }
            } else Double.NaN

            new SimplePredicate(f, operator, value)
          }
        })
        case EvElemStart(_, ElemTags.COMPOUND_PREDICATE, attrs, _)   => makeElem(reader, attrs, new
            ElemBuilder[CompoundPredicate] {
          def build(reader: XMLEventReader, attrs: XmlAttrs): CompoundPredicate = {
            val booleanOperator = CompoundPredicate.BooleanOperator.withName(attrs(AttrTags.BOOLEAN_OPERATOR))
            val children: Array[Predicate] = makeElems(reader, ElemTags.COMPOUND_PREDICATE, Predicate.values, new
                GroupElemBuilder[Predicate] {
              def build(reader: XMLEventReader, event: EvElemStart): Predicate = makePredicate(reader, event)
            })

            new CompoundPredicate(booleanOperator, children)
          }
        })
        case EvElemStart(_, ElemTags.SIMPLE_SET_PREDICATE, attrs, _) => makeElem(reader, attrs, new
            ElemBuilder[SimpleSetPredicate] {
          def build(reader: XMLEventReader, attrs: XmlAttrs): SimpleSetPredicate = {
            val f = field(attrs(AttrTags.FIELD))
            val booleanOperator = SimpleSetPredicate.BooleanOperator.withName(attrs(AttrTags.BOOLEAN_OPERATOR))
            val array = makeElem(reader, ElemTags.SIMPLE_SET_PREDICATE, ElemTags.ARRAY,
              new ElemBuilder[Array[_]] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[_] =
                  makeArray(reader, attrs)
              })
            val values = array.get.map(x => f.encode(x)).toSet

            new SimpleSetPredicate(f, booleanOperator, values)
          }
        })
        case EvElemStart(_, ElemTags.TRUE, _, _)                     => True
        case EvElemStart(_, ElemTags.FALSE, _, _)                    => False
        case _                                                       => ??????
      }
    })

  protected def makeContinuousDistribution(reader: XMLEventReader, event: EvElemStart): ContinuousDistribution =
    makeElem(reader, event, new GroupElemBuilder[ContinuousDistribution] {
      override def build(reader: XMLEventReader, event: EvElemStart): ContinuousDistribution = event match {
        case EvElemStart(_, ElemTags.ANY_DISTRIBUTION, attrs, _)      => makeElem(reader, attrs, new
            ElemBuilder[AnyDistribution] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): AnyDistribution = {
            val mean = attrs.double(AttrTags.MEAN)
            val variance = attrs.double(AttrTags.VARIANCE)
            new AnyDistribution(mean, variance)
          }
        })
        case EvElemStart(_, ElemTags.GAUSSIAN_DISTRIBUTION, attrs, _) => makeElem(reader, attrs, new
            ElemBuilder[GaussianDistribution] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): GaussianDistribution = {
            val mean = attrs.double(AttrTags.MEAN)
            val variance = attrs.double(AttrTags.VARIANCE)
            new GaussianDistribution(mean, variance)
          }
        })
        case EvElemStart(_, ElemTags.POISSON_DISTRIBUTION, attrs, _)  => makeElem(reader, attrs, new
            ElemBuilder[PoissonDistribution] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): PoissonDistribution = {
            val mean = attrs.double(AttrTags.MEAN)
            new PoissonDistribution(mean)
          }
        })
        case EvElemStart(_, ElemTags.UNIFORM_DISTRIBUTION, attrs, _)  => makeElem(reader, attrs, new
            ElemBuilder[UniformDistribution] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): UniformDistribution = {
            val lower = attrs.double(AttrTags.LOWER)
            val upper = attrs.double(AttrTags.UPPER)
            new UniformDistribution(lower, upper)
          }
        })
        case _                                                        => ??????
      }
    })

  def makeArray(reader: XMLEventReader, attrs: XmlAttrs): Array[_] = {
    val arrayType = ArrayType.withName(attrs(AttrTags.TYPE))
    val n = attrs.getInt(AttrTags.N)

    val text = extractText(reader, ElemTags.ARRAY)
    val a = extractArray(text)
    n.foreach(x => if (x != a.size) throw new SemanticErrorException(s"The length of array must be $x, got ${a.size}"))
    import ArrayType._
    arrayType match {
      case `int`    => {
        val result = new Array[Long](a.size)
        for (i <- 0 until a.size) {
          result(i) = a(i).toLong
        }
        result
      }
      case `real`   => {
        val result = new Array[Double](a.size)
        for (i <- 0 until a.size) {
          result(i) = a(i).toDouble
        }
        result
      }
      case `string` => {
        a
      }
    }
  }

  def makeRealArray(reader: XMLEventReader, attrs: XmlAttrs): Array[Double] = {
    val res = makeArray(reader, attrs)
    res.array.asInstanceOf[Array[Double]]
  }

  def makeIntArray(reader: XMLEventReader, attrs: XmlAttrs): Array[Int] = {
    val res = makeArray(reader, attrs)
    res.array.asInstanceOf[Array[Int]]
  }

  def makeStringArray(reader: XMLEventReader, attrs: XmlAttrs): Array[String] = {
    val res = makeArray(reader, attrs)
    res.array.asInstanceOf[Array[String]]
  }

  def makeRealSparseArray(reader: XMLEventReader, attrs: XmlAttrs): SparseVector[Double] = {
    val n = attrs.getInt(AttrTags.N)
    val defaultValue = attrs.getDouble(AttrTags.DEFAULT_VALUE, 0.0)
    val (indices, entries) = makeElem(reader, ElemTags.REAL_SPARSE_ARRAY, ElemTags.INDICES, new
        ElemBuilder[Array[Int]] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[Int] = {
        val text = extractText(reader, ElemTags.INDICES)
        ArrayUtils.toInt(extractArray(text))
      }
    }, ElemTags.REAL_ENTRIES, new ElemBuilder[Array[Double]] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[Double] = {
        val text = extractText(reader, ElemTags.REAL_ENTRIES)
        ArrayUtils.toDouble(extractArray(text))
      }
    })

    if (indices.isDefined && entries.isDefined) {
      if (indices.get.length != entries.get.length) {
        throw new PmmlException("Both arrays, Indices and INT-Entries or REAL-Entries, must have the same length")
      } else {
        val len = n.getOrElse(indices.get.last)
        new SparseVector[Double](len, indices.get.map(_ - 1), entries.get, defaultValue)
      }
    } else {
      new SparseVector[Double](n.get, Array.emptyIntArray, Array.emptyDoubleArray, defaultValue)
    }
  }

  def makeIntSparseArray(reader: XMLEventReader, attrs: XmlAttrs): SparseVector[Int] = {
    val n = attrs.getInt(AttrTags.N)
    val defaultValue = attrs.getInt(AttrTags.DEFAULT_VALUE, 0)
    val (indices, entries) = makeElem(reader, ElemTags.REAL_SPARSE_ARRAY, ElemTags.INDICES, new
        ElemBuilder[Array[Int]] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[Int] = {
        val text = extractText(reader, ElemTags.INDICES)
        ArrayUtils.toInt(extractArray(text))
      }
    }, ElemTags.INT_ENTRIES, new ElemBuilder[Array[Int]] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[Int] = {
        val text = extractText(reader, ElemTags.INT_ENTRIES)
        ArrayUtils.toInt(extractArray(text))
      }
    })

    if (indices.isDefined && entries.isDefined) {
      if (indices.get.length != entries.get.length) {
        throw new PmmlException("Both arrays, Indices and INT-Entries or REAL-Entries, must have the same length")
      } else {
        val len = n.getOrElse(indices.get.last)
        new SparseVector[Int](len, indices.get.map(_ - 1), entries.get, defaultValue)
      }
    } else {
      new SparseVector[Int](n.get, Array.emptyIntArray, Array.emptyIntArray, defaultValue)
    }
  }

  def makeMatrix(reader: XMLEventReader, attrs: XmlAttrs): Matrix = {
    val kind = attrs.get(AttrTags.KIND).map(MatrixKind.withName(_)).getOrElse(MatrixKind.any)
    val nbRows = attrs.getInt(AttrTags.NB_ROWS)
    val nbCols = attrs.getInt(AttrTags.NB_COLS)
    val diagDefault = attrs.getDouble(AttrTags.DIAG_DEFAULT)
    val offDiagDefault = attrs.getDouble(AttrTags.OFF_DIAG_DEFAULT)
    val arrays = mutable.ArrayBuilder.make[Array[Double]]
    nbRows.foreach(arrays.sizeHint(_))
    val matCells = mutable.ArrayBuilder.make[MatCell]

    traverseElems(reader, ElemTags.MATRIX, {
      case EvElemStart(_, ElemTags.ARRAY, attrs, _)    => arrays += makeRealArray(reader, attrs)
      case EvElemStart(_, ElemTags.MAT_CELL, attrs, _) => matCells += makeElem(reader, attrs, new ElemBuilder[MatCell] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): MatCell = {
          val row = attrs.int(AttrTags.ROW)
          val col = attrs.int(AttrTags.COL)
          val value = extractText(reader, ElemTags.MAT_CELL).toDouble

          new MatCell(row, col, value)
        }
      })
    })

    val as = arrays.result()
    import MatrixKind._
    kind match {
      case `diagonal`  => new DiagonalMatrix(as(0), offDiagDefault)
      case `symmetric` => new SymmetricMatrix(as)
      case `any`       => {
        if (as.length > 0) {
          new DenseMatrix(as)
        } else {
          val cells = matCells.result()
          SparseMatrix.fromCells(nbRows.getOrElse(cells.maxBy(_.row).row),
            nbCols.getOrElse(cells.maxBy(_.col).col),
            cells,
            diagDefault,
            offDiagDefault)
        }
      }
    }
  }

  protected def makeModelStats(reader: XMLEventReader): Option[ModelStats] = {
    skipLabel(reader)
    // TODO: ModelStats
    None
  }

  protected def makeModelExplanation(reader: XMLEventReader): Option[ModelExplanation] = {
    skipLabel(reader)
    // TODO: ModelExplanation
    None
  }

  protected def makeModelVerification(reader: XMLEventReader, attrs: XmlAttrs): Option[ModelVerification] = {
    skipLabel(reader)
    // TODO: ModelVerification
    None
  }

  protected def extractArray(text: String): Array[String] = {
    if (text == null || text.isEmpty) {
      return ArrayUtils.emptyStringArray
    }

    val a = mutable.ArrayBuilder.make[String]
    var (begin, end) = (-1, -1)
    var i = 0
    while (i < text.length) {
      val c = text(i)
      if (c == '\n') {
        i += 1
      } else if (c == '\\' && i < text.length - 1 && text(i + 1) == '"') {
        i += 2
      } else {
        if (c == '"') {
          begin = i + 1
          end = begin
          var j = end
          while (j < text.length && (text(j) != '"' || text(j - 1) == '\\')) {
            j += 1
          }
          end = j
          i = j + 1
        } else {
          if (c == ' ' || c == '\t') {
            if (begin >= 0) {
              end = i
            }
          } else {
            if (begin < 0) {
              begin = i
            }

            if (i == text.length - 1) {
              end = text.length
            }
          }
        }

        if (begin >= 0 && end >= 0) {
          a += text.substring(begin, end)
          begin = -1
          end = -1
        }

        i += 1
      }
    }
    a.result()
  }

  def makePartition(reader: XMLEventReader, attrs: XmlAttrs): Partition = {
    skipLabel(reader)
    // TODO: Partition
    null
  }

  def makeComparisonMeasure(reader: XMLEventReader, attrs: XmlAttrs): ComparisonMeasure = makeElem(reader, attrs,
    new ElemBuilder[ComparisonMeasure] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): ComparisonMeasure = {
        val kind = ComparisonMeasureKind.withName(attrs(AttrTags.KIND))
        val compareFunction =
          attrs.get(AttrTags.COMPARE_FUNCTION).map(CompareFunction.withName(_)).getOrElse(CompareFunction.absDiff)
        val minimum = attrs.getDouble(AttrTags.MINIMUM)
        val maximum = attrs.getDouble(AttrTags.MAXIMUM)
        var distance: Distance = null
        traverseElems(reader, ElemTags.COMPARISON_MEASURE, {
          case event: EvElemStart if (Distance.contains(event.label)) => distance = makeElem(reader, event, new
              GroupElemBuilder[Distance] {
            override def build(reader: XMLEventReader, event: EvElemStart): Distance = event match {
              case EvElemStart(_, ElemTags.EUCLIDEAN, _, _)             => euclidean
              case EvElemStart(_, ElemTags.SQUARED_EUCLIDEAN, _, _)     => squaredEuclidean
              case EvElemStart(_, ElemTags.CHEBYCHEV, _, _)             => chebychev
              case EvElemStart(_, ElemTags.CITY_BLOCK, _, _)            => cityBlock
              case EvElemStart(_, ElemTags.MINKOWSKI, attrs, _)         => makeElem(reader, attrs, new
                  ElemBuilder[Distance] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): Distance = new minkowski(attrs.double
                (AttrTags.P_PARAMETER))
              })
              case EvElemStart(_, ElemTags.SIMPLE_MATCHING, _, _)       => simpleMatching
              case EvElemStart(_, ElemTags.JACCARD, _, _)               => jaccard
              case EvElemStart(_, ElemTags.TANIMOTO, _, _)              => tanimoto
              case EvElemStart(_, ElemTags.BINARY_SIMILARITY, attrs, _) => makeElem(reader, attrs, new
                  ElemBuilder[Distance] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): Distance = {
                  new binarySimilarity(attrs.double(AttrTags.C00_PARAMETER),
                    attrs.double(AttrTags.C01_PARAMETER),
                    attrs.double(AttrTags.C10_PARAMETER),
                    attrs.double(AttrTags.C11_PARAMETER),
                    attrs.double(AttrTags.D00_PARAMETER),
                    attrs.double(AttrTags.D01_PARAMETER),
                    attrs.double(AttrTags.D10_PARAMETER),
                    attrs.double(AttrTags.D11_PARAMETER))
                }
              })
              case _                                                    => ??????
            }
          })
        })

        new ComparisonMeasure(kind, distance, compareFunction, minimum, maximum)
      }
    })
}

object Builder {

  val builders: mutable.Map[String, Class[_ <: Builder[_ <: Model]]] = mutable.Map.empty

  def register(name: String, cls: Class[_ <: Builder[_ <: Model]]): Class[_ <: Builder[_ <: Model]] = {
    builders.put(name, cls).getOrElse(cls)
  }

  def unregister(name: String) = {
    builders.remove(name)
  }

  def get(name: String): Option[Builder[_ <: Model]] = builders.get(name).map {
    x => x.getDeclaredConstructor().newInstance().asInstanceOf[Builder[_ <: Model]]
  }

  // register all candidate model builders
  register(ElemTags.TREE_MODEL, classOf[TreeBuilder])
  register(ElemTags.REGRESSION_MODEL, classOf[RegressionBuilder])
  register(ElemTags.MINING_MODEL, classOf[MiningBuilder])
  register(ElemTags.NEURAL_NETWORK, classOf[NeuralNetworkBuilder])
  register(ElemTags.NAIVE_BAYES_MODEL, classOf[NaiveBayesBuilder])
  register(ElemTags.SUPPORT_VECTOR_MACHINE_MODEL, classOf[SupportVectorMachineBuilder])
  register(ElemTags.CLUSTERING_MODEL, classOf[ClusteringBuilder])
  register(ElemTags.GENERAL_REGRESSION_MODEL, classOf[GeneralRegressionBuilder])
  register(ElemTags.ASSOCIATION_MODEL, classOf[AssociationBuilder])
  register(ElemTags.RULE_SET_MODEL, classOf[RuleSetBuilder])
  register(ElemTags.NEAREST_NEIGHBOR_MODEL, classOf[NearestNeighborBuilder])
  register(ElemTags.SCORECARD, classOf[ScorecardBuilder])
  register(ElemTags.ANOMALY_DETECTION_MODEL, classOf[AnomalyDetectionBuilder])
}

trait ElemBuilder[T] {
  def build(reader: XMLEventReader, attrs: XmlAttrs): T
}

trait GroupElemBuilder[T] {
  def build(reader: XMLEventReader, event: EvElemStart): T
}


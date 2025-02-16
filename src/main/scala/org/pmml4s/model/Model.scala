/*
 * Copyright (c) 2017-2024 AutoDeployAI
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
package org.pmml4s.model

import java.io.{File, InputStream}
import java.nio.file.Path
import org.pmml4s.common._
import org.pmml4s.data.{DSeries, DataVal, GenericMutableSeriesWithSchema, GenericSeriesWithSchema, JoinedSeries, NullSeries, Series}
import org.pmml4s.metadata.UsageType.UsageType
import org.pmml4s.metadata._
import org.pmml4s.transformations.{FieldRef, HasLocalTransformations, TransformationDictionary}
import org.pmml4s.util.{ArrayUtils, Utils}
import org.pmml4s.xml.ModelBuilder

import scala.collection.mutable
import scala.io.{Codec, Source}
import scala.util.control.Breaks._

/**
 * Abstract class that represents a PMML model
 */
abstract class Model extends HasParent
  with HasVersion
  with HasWrappedModelAttributes
  with HasMiningSchema
  with HasOutput
  with HasModelStats
  with HasModelExplanation
  with HasTargets
  with HasLocalTransformations
  with FieldScope
  with ModelLocation
  with HasTargetFields
  with Predictable
  with HasModelVerification
  with PmmlElement {

  /** All input names in an array. */
  lazy val inputNames: Array[String] = if (miningSchema != null) miningSchema.inputNames else
    ArrayUtils.emptyStringArray

  /** All input fields in an array. */
  lazy val inputFields: Array[Field] = if (isTopLevelModel) inputNames.map(dataDictionary(_)) else
    inputNames.map(field)

  /** Referenced derived fields. */
  lazy val inputDerivedFields: Array[Field] = {
    transformationDictionary.map(_.referencedFields).getOrElse(Array.empty[Field]) ++
      localTransformations.map(_.referencedFields).getOrElse(Array.empty[Field])
  }

  /** Implicit referenced derived fields for the sub-model except ones defined in the mining schema.  */
  lazy val implicitInputDerivedFields: Array[Field] = if (isSubModel) {
    val dfs = parent.inputDerivedFields
    (Set(dfs.toIndexedSeq: _*) -- Set(inputFields.toIndexedSeq: _*)).toArray
  } else Array.empty

  /** All target names in an array. */
  lazy val targetNames: Array[String] = targets.map(_.targetNames).getOrElse(if (miningSchema != null)
    miningSchema.targetNames else ArrayUtils.emptyStringArray)

  /**
   * All target fields in an array. Multiple target fields are allowed.
   * It depends on the kind of the model whether prediction of multiple fields is supported.
   */
  lazy val targetFields: Array[Field] = if (isTopLevelModel) targetNames.map(dataDictionary(_)) else {
    // The name could be absent
    targetNames.map(x => if (x != "") field(x) else null)
  }

  /** The first target field for the supervised model. */
  lazy val targetField: Field = if (targetFields.nonEmpty) targetFields.head else null

  protected val NullOutputsPair: (Series, ModelOutputs) =  (null, null)

  /** Get fields by its usage type: 'active', 'target', 'predicted', 'group' and so on */
  def fieldsOfUsageType(typ: UsageType): Array[Field] = miningSchema.getByUsageType(typ).map(x => field(x.name))

  /**
   * When Target specifies optype then it overrides the optype attribute in a corresponding MiningField, if it exists.
   * If the target does not specify optype then the MiningField is used as default. And, in turn, if the MiningField
   * does not specify an optype, it is taken from the corresponding DataField. In other words, a MiningField overrides
   * a DataField, and a Target overrides a MiningField.
   */
  lazy val opType: OpType = if (targetField != null) {
    targets.flatMap(_.target.optype).getOrElse(miningSchema(targetName).opType.getOrElse(targetField.opType))
  } else if (isRegression) OpType.continuous else if (isClassification) OpType.nominal else OpType.typeless

  /** Returns optype of the specified target. */
  def opType(name: String): OpType = {
    targets.flatMap(_.get(name).flatMap(_.optype)).getOrElse(miningSchema(name).opType.getOrElse(field(name).opType))
  }

  /** The class labels in a classification model. */
  lazy val classes: Array[DataVal] = if (isClassification) {
    if (targetField != null) targets.map(_.categories).getOrElse(targetField.validValues) else inferClasses
  } else ArrayUtils.emptyDataValArray

  /** The sub-classes can override this method to provide classes of target inside model. */
  def inferClasses: Array[DataVal] = {
    if (isClassification) {
      // collect classes from the output fields with probability
      outputFields.filter(x => x.feature == ResultFeature.probability && x.value.isDefined).map(_.value.get)
    } else ArrayUtils.emptyDataValArray
  }

  /** The number of class labels in a classification model. */
  lazy val numClasses: Int = classes.length

  /** The class labels of all categorical targets. */
  lazy val targetClasses: Map[String, Array[DataVal]] = targetFields.filter(_.isCategorical).map(x => {
    (x.name, targets.flatMap(y => y.categories(x.name)).getOrElse(x.validValues))
  }).toMap

  /** The schema of inputs. */
  lazy val inputSchema: StructType = StructType(inputFields.map {
    x => StructField(x.name, x.dataType)
  })

  /** The schema of candidate outputs. */
  def candidateOutputSchema: StructType = StructType(candidateOutputFields.map {
    x => StructField(x.name, x.dataType)
  })

  /** The schema of final outputs. */
  def outputSchema: StructType = StructType(outputFields.map {
    x => StructField(x.name, x.dataType)
  })

  /** Returns class labels of the specified target. */
  def classes(name: String): Array[DataVal] = targetClasses.getOrElse(name, ArrayUtils.emptyDataValArray)

  /** Returns the number of class labels of the specified target. */
  def numClasses(name: String): Int = classes(name).length

  /** Tests if this is a classification model. */
  override def isClassification: Boolean = super.isClassification

  /** Tests if this is a classification model of the specified target, it's applicable for multiple targets. */
  def isClassification(name: String): Boolean = hasTarget && OpType.isCategorical(opType(name))

  /** Tests if this is a regression model. */
  override def isRegression: Boolean = super.isRegression

  /** Tests if this is a regression model of the specified target, it's applicable for multiple targets. */
  def isRegression(name: String): Boolean = hasTarget && OpType.isRegression(opType(name))

  /** Tests if the target is a binary field */
  def isBinary: Boolean = isClassification && {
    // Firstly, check the number of target values if Target is present, then values defined by DataField
    targets.map(x => if (x.hasTarget) x.categories.length == 2 else false).getOrElse({
      if (targetField != null) targetField.isBinary else classes.length == 2
    })
  }

  /** Tests if the target is an ordinal field */
  def isOrdinal: Boolean = singleTarget && OpType.isOrdinal(opType)

  /** Returns importances of predictors. */
  def importances: Map[String, Double] = miningSchema.importances

  /** The header of this model. */
  def header: Header = parent.header

  /** The data dictionary of this model. */
  def dataDictionary: DataDictionary = parent.dataDictionary

  /** The optional transformation dictionary. */
  def transformationDictionary: Option[TransformationDictionary] = parent.transformationDictionary

  /** Model element type. */
  def modelElement: ModelElement

  /** Predicts values for a given data map. */
  def predict(values: Map[String, Any]): Map[String, Any] = {
    predict(Series.fromMap(values, usedSchema)).asMap
  }

  /** Predicts values for a given data map of Java. */
  def predict(values: java.util.Map[String, Any]): java.util.Map[String, Any] = {
    predict(Series.fromMap(values, usedSchema)).asJavaMap
  }

  /** Predicts values for a given list of key/value pairs. */
  def predict(values: (String, Any)*): Seq[(String, Any)] = {
    predict(Series.fromMap(Map(values: _*), usedSchema)).asPairSeq
  }

  /** Predicts values for a given Array, and the order of those values is supposed as same as the input fields list */
  def predict[T](values: Array[T]): Array[Any] = {
    // convert the values based on the required types of inputs
    val len = usedSchema.size
    val convertedValues = new Array[DataVal](len)
    var i = 0
    while (i < len) {
      if (i < values.length) {
        convertedValues(i) = Utils.toDataVal(values(i), usedSchema(i).dataType)
      }
      i += 1
    }
    predict(Series.fromArray(convertedValues)).asArray
  }

  def predict(values: java.util.List[Any]): java.util.List[Any] = {
    val array = predict(values.toArray())
    val result = new java.util.ArrayList[Any](array.length)
    array.foreach(x => result.add(x))
    result
  }
  
  /**
   * Predicts one or multiple records in json format, there are two formats supported:
   *
   * - ‘records’ : list like [{column -> value}, … , {column -> value}]
   * - ‘split’ : dict like {‘columns’ -> [columns], ‘data’ -> [values]}
   *
   * @param json Records in json
   * @return Results in json
   */
  def predict(json: String): String = {
    import spray.json._
    import DefaultJsonProtocol._
    val result: JsValue = json.parseJson match {
      case x: JsArray  => {
        JsArray(x.elements.map(y => {
          val record = y.asJsObject
          val outputs = predict(Series.fromMap(record, usedSchema))
          outputs.toJson()
        }))
      }
      case x: JsObject => {
        val columns = x.fields("columns").asInstanceOf[JsArray].elements.map(_.convertTo[String])
        val data = x.fields("data").asInstanceOf[JsArray]
        var outputColumns: JsArray = null
        val outputData = JsArray(data.elements.map(y => {
          val values = y.asInstanceOf[JsArray]
          val outputs = predict(Series.fromMap(JsObject(columns.zip(values.elements).toMap), usedSchema))
          if (outputColumns == null) outputColumns = JsArray(outputs.columns.toVector.map(JsString(_)))
          outputs.toJson(false)
        }))

        JsObject(("columns" -> outputColumns), ("data" -> outputData))
      }
      case _           => JsNull
    }
    result.compactPrint
  }

  def predict(it: Iterator[Series]): Iterator[Series] = {
    new Iterator[Series] {
      override def hasNext: Boolean = it.hasNext

      override def next(): Series = predict(it.next())
    }
  }

  /** Predicts values for a given data series.
   *
   * @param values An input data series
   * @return An output data series
   */
  def predict(values: Series): Series

  /** Predicts value for a given data series
   *
   * @param values An input data series
   * @param modelOutputs An input model outputs that can hold all results, it will be ignored if its type doesn't
   *                     match the current model.
   * @return An object of model outputs
   */
  def score(values: Series, modelOutputs: Option[ModelOutputs] = None): (Series, ModelOutputs) = NullOutputsPair

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  def createOutputs(): ModelOutputs

  /** Tests if probabilities of categories of target can be produced by this model. */
  def probabilitiesSupported: Boolean = isClassification

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  def defaultOutputFields: Array[OutputField] = {
    if (isClassification || isRegression) {
      val result = mutable.ArrayBuilder.make[OutputField]
      result.sizeHint(if (isClassification) numClasses + 2 else 1)
      result += OutputField.predictedValue(targetField)
      if (probabilitiesSupported) {
        result += OutputField.probability()
        for (cls <- classes) {
          result += OutputField.probability(cls)
        }
      }

      result.result()
    } else {
      Array.empty
    }
  }

  /** Returns the field of a given name, None if a field with the given name does not exist. */
  override def getField(name: String): Option[Field] = {
    localTransformations.flatMap(_.getField(name)) orElse parent.getField(name)
  }

  /** Pre-process the input series. */
  protected def prepare(series: Series): (Series, Boolean) = {
    val fields = usedFields
    val newValues = new Array[DataVal](if (isTopLevelModel) fields.length else series.length)
    val newSchema = if (isTopLevelModel) usedSchema else series.schema

    // Some models do not have the Mining Schema, for example the transformation model, and embedded models
    if (miningSchema != null) {
      var i = 0
      val len = fields.length
      while (i < len) {
        val field = fields(i)

        // If there is no schema in the input series, that means its values can only be accessed by index, not name.
        // Then the order of that series is supposed as same as the used fields list
        val idx = if (field.index >= 0) field.index else series.fieldIndex(field.name)
        if (idx >= 0) {
          val value = series(idx)
          val mf = miningSchema(field.name)
          if (field.isPlain && mf.isDefault) {
            newValues(idx) = value
          } else {
            val missing = (mf.outliers == OutlierTreatmentMethod.asMissingValues && field.isValidValue(value) && {
              val d = series.getDouble(idx)
              d < mf.lowValue.get || d > mf.highValue.get
            }) ||
              (mf.invalidValueTreatment == InvalidValueTreatment.asMissing && field.isInvalidValue(value)) ||
              field.isMissingValue(value)
            val invalid = if (!missing) {
              field.isInvalidValue(value) || !field.isValidValue(value)
            } else false

            newValues(idx) = if (missing) {
              if (mf.missingValueTreatment.contains(MissingValueTreatment.returnInvalid)) {
                return (series, true)
              }

              if (mf.missingValueReplacement.isDefined) {
                mf.missingValueReplacement.get
              } else {
                null
              }
            } else if (invalid) {
              mf.invalidValueTreatment match {
                case InvalidValueTreatment.returnInvalid =>
                  return (series, true)
                case InvalidValueTreatment.asValue =>
                  mf.invalidValueReplacement.get
                case _ => value
              }
            } else {
              if (mf.outliers == OutlierTreatmentMethod.asExtremeValues) {
                val d = series.getDouble(idx)
                if (d < mf.lowValue.get) DataVal.from(mf.lowValue.get) else if (d > mf.highValue.get) DataVal.from(mf.highValue.get) else value
              } else value
            }
          }
        }
        i += 1
      }
    } else {
      var i = 0
      val len = fields.length
      while (i < len) {
        val field = fields(i)
        val idx = if (field.index >= 0) field.index else series.fieldIndex(field.name)
        if (idx >= 0) {
          val value = series(idx)
          val missing = !field.isValidValue(value)
          newValues(idx) = if (missing) {
            null /*value*/
          } else {
            value
          }
        }
        i += 1
      }
    }

    val transformed = if (isTopLevelModel && parent != null) {
      // Compute the values of all referenced derived fields for the top level model
      parent.predict(new GenericSeriesWithSchema(newValues, newSchema))
    } else {
      // Copy the values of referenced derived fields of its parent model
      var i = 0
      val len = implicitInputDerivedFields.length
      while (i < len) {
        val df = implicitInputDerivedFields(i)
        if (df.index >= 0) {
          newValues(df.index) = series(df.index)
        }
        i += 1
      }
      new GenericSeriesWithSchema(newValues, newSchema)
    }

    (localTransformations.map(_.transform(transformed)).getOrElse(transformed), false)
  }

  /** Encodes the input series. */
  protected def encode(series: Series): DSeries = {
    val values = Array.fill(series.size)(Double.NaN)
    var i = 0
    while (i < inputFields.length) {
      val field = inputFields(i)
      if (field.indexDefined) {
        values(field.index) = field.encode(series(field.index))
      }
      i += 1
    }

    DSeries.fromArray(values)
  }

  /** Returns true if there are any missing values of all input fields in the specified series. */
  protected def anyMissing(series: Series): Boolean = {
    for (field <- inputFields) {
      if (!field.indexDefined || !field.isValidValue(series(field.index))) return true
    }
    false
  }

  protected def result(series: Series, modelOutputs: ModelOutputs, fields: Array[OutputField] = Array.empty): Series = {
    if (targets.isDefined) {
      modelOutputs match {
        case multiModelOutputs: MultiModelOutputs     => {
          for (name <- targetNames) {
            multiModelOutputs.get(name).foreach({
              case mutablePredictedValue: MutablePredictedValue => postPredictedValue(mutablePredictedValue, name)
              case _                                            =>
            })
          }
        }
        case setPredictedValue: MutablePredictedValue => {
          postPredictedValue(setPredictedValue)
        }
      }
    }

    val candidateFields = candidateOutputFields
    val outputSeries = new GenericMutableSeriesWithSchema(candidateFields.length, candidateOutputSchema)
    outputSeries.clear()

    import ResultFeature._

    val isMultiple = multiTargets && modelOutputs.isInstanceOf[MultiModelOutputs] 
    
    var i = 0
    val len = candidateFields.length
    while (i < len && (!isMultiple || candidateFields(i).targetField.nonEmpty)) {
      val of = candidateFields(i)

      val outputs = if (isMultiple) {
        modelOutputs.asInstanceOf[MultiModelOutputs](of.targetField.get)
      } else modelOutputs

      // NOT set the index of output fields because the input series could contain values of local derived fields,
      // which are only available within the local scope, and not returned.
      // of.index = series.size + i
      of.feature match {
        case `predictedValue`                                  => outputs match {
          // The expected data type could be different from the storage type of value, so try to convert it
          case x: HasPredictedValue   => outputSeries.update(i,
            if (x.predictedValue.dataType != of.dataType) Utils.toDataVal(x.predictedValue.toVal, of.dataType) else x.predictedValue)
          case x: HasAssociationRules => outputSeries.update(i,
            x.getRule(of.criterion, of.rank).map(_.predictedValue).orNull)
          case _                      =>
        }
        case `predictedDisplayValue`                           => outputs match {
          case x: HasPredictedDisplayValue => outputSeries.update(i, x.predictedDisplayValue)
          case _                           =>
        }
        case `transformedValue`                                => {
          if (of.segmentId.isDefined) {
            if (of.expr.isDefined && of.expr.get.isInstanceOf[FieldRef]) {
              outputs match {
                case x: HasSegment => {
                  val ref = of.expr.get.asInstanceOf[FieldRef]
                  outputSeries.update(i, x.segment(of.segmentId.get).get(ref.field.name))
                }
                case _             =>
              }
            }
          } else if (of.expr.isDefined) {
            val joinedSeries = new JoinedSeries(series, outputSeries)
            of.expr.foreach(x => outputSeries.update(i, x.eval(joinedSeries)))
          }
        }
        case `decision`                                        => {
          if (of.expr.isDefined) {
            val joinedSeries = new JoinedSeries(series, outputSeries)
            of.expr.foreach(x => outputSeries.update(i, x.eval(joinedSeries)))
          } else if (of.segmentId.isDefined) {
            ???
          }
        }
        case `probability`                                     => outputs match {
          case x: HasPredictedValueWithProbabilities => outputSeries.setDouble(i,
            x.probability(of.value.getOrElse(x.predictedValue)))
          case x: HasAssociationRules                => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).map(_.confidence).getOrElse(Double.NaN))
          case _                                     =>
        }
        case `affinity` | `entityAffinity` | `clusterAffinity` => outputs match {
          case x: HasAffinities       => {
            if (of.value.isDefined) {
              outputSeries.setDouble(i, x.affinity(of.value.get))
            } else {
              outputs match {
                case y: HasEntityId  => outputSeries.setDouble(i, x.affinity(y.entityId))
                case y: HasEntityIds => outputSeries.setDouble(i, x.affinity(y.entityId(of.rank)))
                case _               =>
              }
            }
          }
          case x: HasAssociationRules => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).flatMap(_.affinity).getOrElse(Double.NaN))
          case _                      =>
        }
        case `residual`                                        =>
          if (targetField != null && !targetField.isMissing(series)) {
            if (isRegression) {
              outputs match {
                case x: HasPredictedValue => outputSeries.setDouble(i,
                  Utils.toDouble(x.predictedValue) - targetField.getDouble(series))
              }
            } else if (isClassification) {
              outputs match {
                case x: HasPredictedValueWithProbabilities => {
                  val category = of.value.getOrElse(x.predictedValue)
                  outputSeries.setDouble(i,
                    (if (targetField.get(series) == category) 1.0 else 0.0) - x.probability(category))
                }
              }
            }
          }
        case `standardError`                                   => outputs match {
          case x: HasStandardError => outputSeries.setDouble(i, x.standardError)
          case _                   =>
        }
        case `clusterId` | `entityId` | `ruleId`               => outputs match {
          // The expected data type could be different from the storage type of value, so try to convert it
          case x: HasEntityId         => outputSeries.update(i, Utils.toDataVal(x.entityId, of.dataType))
          case x: HasEntityIds        => outputSeries.update(i, Utils.toDataVal(x.entityId(of.rank), of.dataType))
          case x: HasAssociationRules => outputSeries.update(i,
            x.getRule(of.criterion, of.rank).map(_.entityId).getOrElse(DataVal.NULL))
          case _                      =>
        }
        case `warning`                                         => outputs match {
          case x: HasWarning => outputSeries.update(i, x.warning)
          case _             =>
        }
        case `reasonCode`                                      => outputs match {
          case x: HasReasonCode  => outputSeries.update(i, x.reasonCode)
          case x: HasReasonCodes => outputSeries.update(i, x.reasonCode(of.rank))
          case _                 =>
        }
        case `antecedent`                                      => outputs match {
          case x: HasAssociationRules => outputSeries.update(i,
            x.getRule(of.criterion, of.rank).map(_.antecedentRule).orNull)
          case _                      =>
        }
        case `consequent`                                      => outputs match {
          case x: HasAssociationRules => outputSeries.update(i,
            x.getRule(of.criterion, of.rank).map(_.consequentRule).orNull)
          case _                      =>
        }
        case `rule`                                            => outputs match {
          case x: HasAssociationRules => outputSeries.update(i,
            x.getRule(of.criterion, of.rank).map(_.rule).orNull)
          case _                      =>
        }
        case `confidence`                                      => outputs match {
          case x: HasConfidence       => outputSeries.setDouble(i, x.confidence)
          case x: HasAssociationRules => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).map(_.confidence).getOrElse(Double.NaN))
          case _                      =>
        }
        case `support`                                         => outputs match {
          case x: HasAssociationRules => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).map(_.support).getOrElse(Double.NaN))
          case _                      =>
        }
        case `lift`                                            => outputs match {
          case x: HasAssociationRules => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).flatMap(_.lift).getOrElse(Double.NaN))
          case _                      =>
        }
        case `leverage`                                        => outputs match {
          case x: HasAssociationRules => outputSeries.setDouble(i,
            x.getRule(of.criterion, of.rank).flatMap(_.leverage).getOrElse(Double.NaN))
          case _                      =>
        }
        case _                                                 => ???
      }
      i += 1
    }


    // Check if there are intermediate results, which are only for the top level model.
    // For the child model in Mining Model should still output them that could be used by following models.
    if (isSubModel || candidateFields.length == outputFields.length) {
      outputSeries.toSeries
    } else {
      Series.fromSeq(outputSeries.toSeq.zip(candidateFields).filter(_._2.isFinalResult).map(_._1), outputSchema)
    }
  }

  protected def postPredictedValue(outputs: MutablePredictedValue, name: String = null): MutablePredictedValue = {
    if ((name == null && isClassification) || (name != null && isClassification(name))) {
      if (outputs.predictedValue == null) {
        val (predictedValue, probabilities) = postClassification(name)
        outputs.setPredictedValue(predictedValue)

        outputs match {
          case x: MutableProbabilities => x.setProbabilities(probabilities)
          case _                       =>
        }
      }

      if (outputs.predictedValue != null) {
        outputs match {
          case x: MutablePredictedDisplayValue => x.setPredictedDisplayValue(
            targets.get.displayValue(outputs.predictedValue, name).getOrElse(outputs.predictedValue.toString))
          case _                               =>
        }
      }
    } else if ((name == null && isRegression) || (name != null && isRegression(name))) {
      outputs.setPredictedValue(postRegression(outputs.predictedValue, name))
    }

    outputs
  }

  protected def postRegression(predictedValue: DataVal, name: String = null): DataVal = {
    if (targets.isDefined) {
      val ts = targets.get
      val res = if (Utils.isMissing(predictedValue)) {
        (if (ts.singleTarget) {
          ts.defaultValue
        } else {
          ts.get(name).flatMap(_.defaultValue)
        }) getOrElse (Double.NaN)
      } else {
        val dValue = Utils.toDouble(predictedValue)
        if (ts.singleTarget) {
          ts.postPredictedValue(dValue)
        } else {
          ts.get(name).map(_.postPredictedValue(dValue)).getOrElse(Double.NaN)
        }
      }
      DataVal.from(res)
    } else {
      predictedValue
    }
  }

  protected def postClassification(name: String = null): (DataVal, Map[DataVal, Double]) = {
    if (targets.isDefined) {
      val ts = targets.get
      if (ts.singleTarget) {
        (ts.priorPredictedValue, ts.priorProbabilities)
      } else {
        (ts.priorPredictedValue(name), ts.priorProbabilities(name))
      }
    } else {
      (null, Map.empty)
    }
  }

  /** A series with all null values is returned when can not produce a result. */
  lazy val nullSeries: Series = Series.fromArray(new Array[DataVal](outputSchema.size), outputSchema)

  /**
   * Setup indices to retrieve data from series faster by index instead of name, the index is immutable when model is
   * built because the model object could run in multiple threads, so it's important make sure the model object is
   * totally immutable.
   *
   * Setup indices of targets that are usually not used by the scoring process, they are only used when residual
   * values to be computed.
   */
  lazy val usedFields: Array[Field] = if (isTopLevelModel) {
    val ones = inputFields ++ targetFieldsOfResidual
    ones.zipWithIndex.foreach(x => x._1.index = x._2)
    ones
  } else inputFields

  /** The schema of used fields. */
  lazy val usedSchema: StructType = StructType(usedFields.map {
    x => StructField(x.name, x.dataType)
  })
}

object Model {

  implicit val codec: Codec = Codec.UTF8

  /** Helper method for loading a model from PMML in a string. */
  def fromString(s: String): Model = apply(Source.fromString(s))

  /** Helper method for loading a model from PMML file with given pathname. */
  def fromFile(name: String): Model = apply(Source.fromFile(name))

  /** Helper method for loading a model from PMML file with given the Java file object. */
  def fromFile(file: File): Model = apply(Source.fromFile(file))

  /** Helper method for loading a model from PMML file with given the Java path object. */
  def fromPath(path: Path): Model = fromFile(path.toFile)

  /** Helper method for loading a model from PMML in array of bytes. */
  def fromBytes(bytes: Array[Byte]): Model = apply(Source.fromBytes(bytes))

  /** Helper method for loading a model from PMML in an inputs stream. */
  def fromInputStream(is: InputStream): Model = apply(Source.fromInputStream(is));

  def apply(src: Source): Model = {
    try {
      ModelBuilder.fromXml(src)
    } finally {
      src.close()
    }
  }
}


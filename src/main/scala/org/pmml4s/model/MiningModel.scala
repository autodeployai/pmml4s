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

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common.Predication.Predication
import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata._
import org.pmml4s.model.MissingPredictionTreatment.MissingPredictionTreatment
import org.pmml4s.model.MultipleModelMethod.MultipleModelMethod
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.{MathUtils, Utils}

import scala.collection._

/**
 * The element MiningModel allows precise specification of the usage of multiple models within one PMML file.
 * The two main approaches are Model Composition, and Segmentation.
 *
 * Model Composition includes model sequencing and model selection but is only applicable to Tree and Regression models.
 * Segmentation allows representation of different models for different data segments and also can be used for model
 * ensembles and model sequences. Scoring a case using a model ensemble consists of scoring it using each model
 * separately, then combining the results into a single scoring result using one of the pre-defined combination methods.
 * Scoring a case using a sequence, or chain, of models allows the output of one model to be passed in as input to
 * subsequent models.
 *
 * ModelComposition uses "embedded model elements" that are defeatured copies of "standalone model elements" --
 * specifically, Regression for RegressionModel, DecisionTree for TreeModel. Besides being limited to Regression and
 * Tree models, these embedded model elements lack key features like a MiningSchema (essential to manage scope across
 * multiple model elements). Therefore, in PMML 4.2, the Model Composition approach has been deprecated since the
 * Segmentation approach allows for a wider range of models to be used more reliably. For more on deprecation, see
 * Conformance.
 *
 * Segmentation is accomplished by using any PMML model element inside of a Segment element, which also contains a
 * PREDICATE and an optional weight. MiningModel then contains Segmentation element with a number of Segment elements
 * as well as the attribute multipleModelMethod specifying how all the models applicable to a record should be combined.
 * It is also possible to use a combination of model composition and segmentation approaches, using simple regression or
 * decision trees for data preprocessing before segmentation.
 */
class MiningModel(
                   var parent: Model,
                   override val attributes: ModelAttributes,
                   override val miningSchema: MiningSchema,
                   val embeddedModels: Array[EmbeddedModel],
                   val segmentation: Option[Segmentation],
                   override val output: Option[Output] = None,
                   override val targets: Option[Targets] = None,
                   override val localTransformations: Option[LocalTransformations] = None,
                   override val modelStats: Option[ModelStats] = None,
                   override val modelExplanation: Option[ModelExplanation] = None,
                   override val modelVerification: Option[ModelVerification] = None,
                   override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedModelAttributes {

  import MultipleModelMethod._

  require(segmentation.isEmpty || MultipleModelMethod.support(segmentation.get.multipleModelMethod, functionName),
    s"The multiple model method '${segmentation.get.multipleModelMethod}' is not supported by the ${functionName} model")

  segmentation.foreach(_.setParent(this))
  private val segmentOutputs = candidateOutputFields.filter(_.segmentId.isDefined).map(_.segmentId.get).toSet
  private val isWeighted = isSegmentation && (Set(`weightedMajorityVote`, `weightedAverage`, `weightedMedian`, `weightedSum`)
    contains segmentation.get.multipleModelMethod)

  // enable the predicted value if it's not
  if (isSegmentation &&
    (!Set(`selectFirst`, `selectAll`, `modelChain`).contains(segmentation.get.multipleModelMethod)) &&
    (isRegression || ((isClassification || isClustering) && (segmentation.get.multipleModelMethod == majorityVote || segmentation.get.multipleModelMethod == weightedMajorityVote)))) {
      segmentation.get.segments.foreach(x =>{
        val idx = x.model.outputIndex(ResultFeature.predictedValue)
        if (idx == -1) {
          x.model.setSupplementOutput(true)
        }
      })
  }

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.MiningModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs()

    // Segmentation
    if (isSegmentation) {
      val seg = segmentation.get
      val segments = seg.segments
      import MissingPredictionTreatment._
      seg.multipleModelMethod match {
        case `selectFirst` => {
          val first = segments.find(x => Predication.fire(x.eval(series)))
          if (first.isDefined) first.get.predict(series) else nullSeries
        }
        case `selectAll`   => {
          val all = segments.map(x => if (Predication.fire(x.eval(series))) x.predict(series) else
            x.model.nullSeries)
          Series.merge(all)
        }
        case `modelChain`  => {
          var last: Series = nullSeries
          var lastOutputFields: Array[OutputField] = Array.empty
          var in = series
          var i = 0
          while (i < segments.length) {
            val segment = segments(i)
            val out = if (Predication.fire(segment.eval(in))) {
              last = segment.predict(in)
              lastOutputFields = segment.model.outputFields
              last
            } else {
              segment.model.nullSeries
            }
            segment.id.foreach(x => if (segmentOutputs.contains(x)) outputs.putSegment(x, out))
            in = Series.merge(in, out)
            i += 1
          }

          if (output.isDefined) {
            // Handle the results further when outputs defined in the ensemble model
            val probabilities = mutable.Map.empty[DataVal, Double]
            last.toSeq.zip(lastOutputFields).foreach(x => {
              x._2.feature match {
                case ResultFeature.predictedValue => outputs.predictedValue = x._1
                case ResultFeature.probability    => x._2.value.foreach(y => {
                  probabilities += (y -> x._1.toDouble)
                })
                case _ =>
              }
            })
            outputs.probabilities = probabilities.toMap

            // Derive the predicted value from probabilities if it's not set
            if (outputs.probabilities.nonEmpty && outputs.predictedValue == null) {
              outputs.evalPredictedValueByProbabilities()
            }
            result(last, outputs)
          } else last
        }
        case method        => {
          val selections = seg.filter(series)
          if (selections.isEmpty) return nullSeries

          if (isRegression || ((isClassification || isClustering) && (method == majorityVote || method == weightedMajorityVote))) {
            // The child model could have different output schema
            val outSeries = selections.map(x => (x.predict(series), x))
            var predictions = outSeries.map(x => x._1.get(x._2.model.predictedValueIndex))

            // Compute model weights
            var weights = if (isWeighted) outSeries.map(x => x._2.weight(Series.merge(series, x._1))) else Array.fill(selections.length)(1.0)

            // Check missing results
            seg.missingPredictionTreatment match {
              case `returnMissing` => if (predictions.exists(x => Utils.isMissing(x))) return nullSeries
              case `skipSegment`   => {
                if (predictions.exists(x => Utils.isMissing(x))) {
                  val missingWeight = weights.zip(predictions).filter(x => Utils.isMissing(x._2)).map(x => x._1).sum
                  val totalWeight = weights.sum
                  if (missingWeight / totalWeight > seg.missingThreshold) return nullSeries
                  weights = predictions.zip(weights).filter(x => Utils.nonMissing(x._1)).map(_._2)
                  predictions = predictions.filter(x => Utils.nonMissing(x))
                }
              }
              case `continue`      =>
            }

            if (isRegression) {
              val realPredictions = predictions.map(_.toDouble)
              val predictedValue = method match {
                case `average`         => {
                  realPredictions.sum / realPredictions.length.toDouble
                }
                case `weightedAverage` => {
                  MathUtils.product(realPredictions, weights) / weights.sum
                }
                case `median`          => {
                  MathUtils.median(realPredictions)
                }
                case `weightedMedian` | `x-weightedMedian`  => {
                  MathUtils.weightedMedian(realPredictions, weights)
                }
                case `sum`             =>
                  realPredictions.sum
                case `weightedSum` | `x-weightedSum`     =>
                  MathUtils.product(realPredictions, weights)
              }
              outputs.setPredictedValue(predictedValue)
            } else {
              // Convert predictions to the data type of target, because its type could be different from its target
              // in some cases, the probabilities could all become 0 if both data types not match.
              val dataTypeWanted = if (classes.nonEmpty) Utils.inferDataType(classes(0)) else UnresolvedDataType
              val probabilities = Utils.reduceByKey(predictions.zip(weights)).map(x =>
                (Utils.toDataVal(x._1, dataTypeWanted), x._2 / predictions.length)).withDefaultValue(0.0)
              outputs.setProbabilities(classes.map(x => (x, probabilities(x))).toMap).evalPredictedValueByProbabilities()
            }
          } else if (isClassification) {
            // Suppose all child model has the same output schema
            val outSeries = selections.map(_.predict(series))
            val probabilityIndexes = classes.map(x => selections.head.model.outputIndex(ResultFeature.probability, Some(x)))

            // Get the probabilities of each class
            var probabilities = outSeries.map(x => probabilityIndexes.map(y => if (y != -1) x.get(y).toDouble else Double.NaN))

            // Compute model weights
            var weights = if (isWeighted) outSeries.zip(selections).map(x => x._2.weight(Series.merge(series, x._1))) else Array.fill(selections.length)(1.0)

            // Check missing results
            seg.missingPredictionTreatment match {
              case `returnMissing` => if (probabilities.exists(x => Utils.anyMissing(x))) return nullSeries
              case `skipSegment`   => {
                if (probabilities.exists(x => Utils.anyMissing(x))) {
                  val missingWeight = weights.zip(probabilities).filter(x => Utils.anyMissing(x._2)).map(x => x._1).sum
                  val totalWeight = weights.sum
                  if (missingWeight / totalWeight > seg.missingThreshold) return nullSeries
                  weights = probabilities.zip(weights).filter(x => !Utils.anyMissing(x._1)).map(_._2)
                  probabilities = probabilities.filter(x => !Utils.anyMissing(x))
                }
              }
              case `continue`      =>
            }

            var evalPredictedValue = true
            val matrix = probabilities.transpose
            outputs.setProbabilities(method match {
              case `average`         => {
                classes.zip(matrix.map(_.sum / selections.length)).toMap
              }
              case `weightedAverage` => {
                val sum = weights.sum
                classes.zip(matrix.map(x => MathUtils.product(x, weights) / sum)).toMap
              }
              case `max`             => {
                evalPredictedValue = false
                outputs.predictedValue = classes.zip(matrix.map(_.max)).maxBy(_._2)._1
                val contributions = probabilities.filter(x => classes.zip(x).maxBy(_._2)._1 == outputs.predictedValue)
                classes.zip(contributions.transpose.map(_.sum / contributions.length)).toMap
              }
              case `median`          => {
                classes.zip(matrix.map(x => MathUtils.median(x))).toMap
              }
              case `weightedMedian` | `x-weightedMedian`  => {
                classes.zip(matrix.map(x => MathUtils.weightedMedian(x, weights))).toMap
              }
            })

            if (evalPredictedValue) {
              outputs.evalPredictedValueByProbabilities()
            }            
          }

          result(series, outputs)
        }
      }
    } else {
      // Model Composition
      ???
    }
  }


  /** Returns the field of a given name, None if a field with the given name does not exist. */
  override def getField(name: String): Option[Field] = {
    if (isSegmentation && segmentation.get.multipleModelMethod == MultipleModelMethod.modelChain) {
      var i = 0
      while (i < segmentation.get.segments.length) {
        val segment = segmentation.get.segments(i)
        val f = segment.model.output.flatMap(_.getField(name))
        if (f.isDefined) return f
        i += 1
      }
    }

    super.getField(name)
  }

  /** Creates an object of MiningOutputs that is for writing into an output series.  */
  override def createOutputs(): MiningOutputs = new MiningOutputs

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override def defaultOutputFields: Array[OutputField] = {
    if (isSegmentation) {
      import MultipleModelMethod._
      segmentation.get.multipleModelMethod match {
        case `modelChain` if segmentation.get.segments.last.predicate == True => {
          val lastModel = segmentation.get.segments.last.model
          lastModel.setSupplementOutput(supplementOutput)
          lastModel.candidateOutputFields
        }
        case `selectAll`                                                      => {
          segmentation.get.segments.flatMap(_.model.candidateOutputFields)
        }
        case _                                                                => {
          super.defaultOutputFields
        }
      }
    }
    else {
      // TODO ModelComposition
      ???
    }
  }

  def isSegmentation: Boolean = segmentation.isDefined
}

/**
 * Specifying how all the models applicable to a record should be combined.
 */
object MultipleModelMethod extends Enumeration {
  type MultipleModelMethod = Value

  /**
   * selectFirst is applicable to any model type. Simply use the first model for which the predicate in the Segment evaluates to true.
   * selectAll is applicable to any model type. All models for which the predicate in the Segment evaluates to true are evaluated.
   * modelChain is applicable to any model type. During scoring, Segments whose Predicates evaluate to TRUE are executed in
   * the order they appear in the PMML.
   *
   * For clustering models only majorityVote, weightedMajorityVote, modelChain, selectFirst, or selectAll can be used.
   * In case of majorityVote the cluster ID that was selected by the largest number of models wins. For weightedMajorityVote
   * the weights specified in Segment elements are used, and the cluster ID with highest total weight wins.
   * Cluster affinity for the resulting model combined by majorityVote or weightedMajorityVote is not defined.
   * Note that combining clustering model predictions can produce not very meaningful results because there is no target variable,
   * and the same cluster IDs on different segments can be assigned to very different clusters.
   *
   * For regression models only average, weightedAverage, median, weightedMedian, sum, weightedSum, modelChain, selectFirst, or selectAll are applicable.
   * The first four methods are applied to the predicted values of all models for which the predicate evaluates to true.
   *
   * For classification models all the combination methods, except for sum and weightedSum, can be used. For the first six combination methods
   * the models in all segments must have the same target variable. Note that average, weightedAverage, median, weightedMedian, and max are
   * applied to the predicted probabilities of target categories in each of the models used for the case, then the winning
   * category is selected based on the highest combined probability, while majorityVote and weightedMajorityVote use the
   * predicted categories from all applicable models and select the one based on the models' "votes". Predicted probabilities
   * for the final prediction of a classification MiningModel are defined as follows, depending on the combination method:
   *
   * - majorityVote and weightedMajorityVote: the probabilities are computed as the proportions of the votes or weighted votes for each target category;
   * - average, weightedAverage: the probabilities are computed as the average or weighted average of probabilities of the models used in the prediction;
   * - max: consider the model(s) that have contributed the chosen probability for the winning category. Return their average probabilities;
   * - median, weightedMedian: if the number of models with predicates that resolve to true is odd consider the model(s) that have the chose
   */
  val majorityVote, weightedMajorityVote, average, weightedAverage, median, weightedMedian, max, sum, weightedSum, selectFirst, selectAll, modelChain = Value
  val `x-weightedMedian`= Value("x-weightedMedian")
  val `x-weightedSum` = Value("x-weightedSum")

  def support(mmm: MultipleModelMethod, mf: MiningFunction): Boolean = mmm match {
    case `selectFirst` | `selectAll` | `modelChain` => true
    case `majorityVote` | `weightedMajorityVote`
      if mf == MiningFunction.clustering            => true
    case `average` | `weightedAverage` | `median` | `weightedMedian` | `x-weightedMedian` | `sum` | `weightedSum` | `x-weightedSum`
      if mf == MiningFunction.regression            => true
    case `majorityVote` | `weightedMajorityVote` | `average` | `weightedAverage` | `median` | `weightedMedian` | `x-weightedMedian` | `max`
      if mf == MiningFunction.classification        => true
    case _                                          => false
  }
}

/**
 * The missing prediction treatment options are used when at least one model for which the predicate in the Segment
 * evaluates to true has a missing result. The attribute missingThreshold is closely related and has default value 1.
 * The options are defined as follows:
 *
 * - returnMissing means that if at least one model has a missing result, the whole MiningModel's result should be missing.
 * - skipSegment says that if a model has a missing result, that segment is ignored and the results are computed based
 * on other segments. However, if the fraction of the models with missing results ( weighted if the model combination
 * method is weighted ) exceeds the missingThreshold, the returned result must be missing. This option should not be
 * used with modelChain combination method.
 * - continue says that if a model has a missing result, the processing should continue normally. This can work well for
 * voting or modelChain situations, as well as returnFirst and returnAll. In case of majorityVote or
 * weightedMajorityVote the missing result can be returned if it gets the most ( possibly weighted ) votes, or if the
 * fraction of the models with missing result exceeds the missingThreshold. Otherwise a valid result is computed
 * normally. Other model combination methods will return a missing value as the result.
 */
object MissingPredictionTreatment extends Enumeration {
  type MissingPredictionTreatment = Value
  val returnMissing, skipSegment, continue = Value
}

class Segmentation(val multipleModelMethod: MultipleModelMethod,
                   val segments: Array[Segment],
                   val missingPredictionTreatment: MissingPredictionTreatment = MissingPredictionTreatment.continue,
                   val missingThreshold: Double = 1) extends PmmlElement {
  private val allTrue: Boolean = segments.count(x => x.isTrue) == segments.length

  def +=(segment: Segment): Segmentation = {
    new Segmentation(multipleModelMethod, segments :+ segment)
  }

  def setParent(parent: Model): Segmentation = {
    segments.foreach(x => x.model.setParent(parent))
    this
  }

  def filter(series: Series): Array[Segment] = if (allTrue) segments else {
    segments.filter(x => Predication.fire(x.eval(series)))
  }
}

class VariableWeight(val field: Field) extends PmmlElement

class Segment(val predicate: Predicate,
              val model: Model,
              val variableWeight: Option[VariableWeight] = None,
              val id: Option[String] = None,
              val weight: Double = 1.0)
  extends Predictable with Predicate with PmmlElement {

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = model.predict(values)

  /** Evaluates the predicate. */
  override def eval(series: Series): Predication = predicate.eval(series)

  def weight(series: Series): Double = variableWeight.map(x => x.field.getDouble(series) * weight).getOrElse(weight)

  override def isTrue: Boolean = predicate.isTrue
}

class MiningOutputs extends ClsOutputs with RegOutputs with CluOutputs with SegmentOutputs {
  override def modelElement: ModelElement = ModelElement.MiningModel

  override def clear(): this.type = {
    super[ClsOutputs].clear()
    super[RegOutputs].clear()
    super[CluOutputs].clear()
    super[SegmentOutputs].clear()
    this
  }
}


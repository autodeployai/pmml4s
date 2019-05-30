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
package org.pmml4s.model

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common.Predication.Predication
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata._
import org.pmml4s.model.MultipleModelMethod.MultipleModelMethod
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.{MathUtils, Utils}

import scala.collection.immutable

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
                   override var parent: Model,
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

  require(segmentation.isEmpty || MultipleModelMethod.support(segmentation.get.multipleModelMethod, functionName),
    s"The multiple model method '${segmentation.get.multipleModelMethod}' is not supported by the ${functionName} model")

  segmentation.foreach(_.setParent(this))
  private val segmentOutputs = candidateOutputFields.filter(_.segmentId.isDefined).map(_.segmentId.get).toSet

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.MiningModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val series = prepare(values)

    val outputs = createOutputs()

    // Segmentation
    if (isSegmentation) {
      val seg = segmentation.get
      import MultipleModelMethod._
      seg.multipleModelMethod match {
        case `selectFirst` => {
          val first = seg.segments.find(x => Predication.fire(x.eval(series)))
          if (first.isDefined) first.get.predict(series) else nullSeries
        }
        case `selectAll`   => {
          val all = seg.segments.map(x => if (Predication.fire(x.eval(series))) x.predict(series) else
            x.model.nullSeries)
          Series.merge(all: _*)
        }
        case `modelChain`  => {
          var last: Series = nullSeries
          var in = series
          for (segment <- seg.segments) {
            val out = if (Predication.fire(segment.eval(in))) {
              last = segment.predict(in)
              last
            } else {
              segment.model.nullSeries
            }
            segment.id.foreach(x => if (segmentOutputs.contains(x)) outputs.putSegment(x, out))
            in = Series.merge(in, out)
          }
          // TODO we need to handle the results further when outputs defined in the ensemble model
          last
        }
        case method        => {
          val selections = seg.segments.filter(x => Predication.fire(x.eval(series)))
          if (selections.isEmpty) return nullSeries

          if (isRegression || ((isClassification || isClustering) && (method == majorityVote || method == weightedMajorityVote))) {
            // Suppose all child model has the same output schema
            val predictedIndex = selections.head.model.outputIndex(ResultFeature.predictedValue)
            val predictions = selections.map(_.predict(series)).map(_.get(predictedIndex))

            // Check missing results
            if (predictions.contains(null)) return nullSeries

            if (isRegression) {
              val realPredictions = predictions.map(_.asInstanceOf[Double])
              outputs.predictedValue = method match {
                case `average`         => {
                  realPredictions.sum / realPredictions.size.toDouble
                }
                case `weightedAverage` => {
                  val weights = selections.map(_.weight)
                  MathUtils.product(realPredictions, weights) / weights.sum
                }
                case `median`          => {
                  if (realPredictions.size % 2 == 0) {
                    (realPredictions(realPredictions.size / 2) + realPredictions(realPredictions.size / 2 - 1)) / 2.0
                  } else {
                    realPredictions(realPredictions.size / 2)
                  }
                }
                case `sum`             =>
                  realPredictions.sum
              }
            } else {
              val weights = if (method == weightedMajorityVote) selections.map(_.weight) else Array.fill(selections.size)(1.0)
              val probabilities = Utils.reduceByKey(predictions.zip(weights)).map(x => (x._1, x._2 / predictions.size)).toMap.withDefaultValue(0.0)
              outputs.setProbabilities(classes.map(x => (x, probabilities(x))).toMap).evalPredictedValueByProbabilities()
            }
          } else if (isClassification) {
            // Suppose all child model has the same output schema
            val probabilityIndexes = classes.map(x => selections.head.model.outputIndex(ResultFeature.probability, Some(x)))
            val m = selections.
              map(_.predict(series)).
              map(x => probabilityIndexes.map(y => if (y != -1) x.get(y).asInstanceOf[Double] else Double.NaN))
            val matrix = m.transpose

            outputs.probabilities = method match {
              case `average`         => {
                classes.zip(matrix.map(_.sum / selections.size)).toMap
              }
              case `weightedAverage` => {
                val weights = selections.map(_.weight)
                val sum = weights.sum
                classes.zip(matrix.map(x => MathUtils.product(x, weights) / sum)).toMap
              }
              case `max`             => {
                outputs.predictedValue = classes.zip(matrix.map(_.max)).maxBy(_._2)
                val contributions = m.filter(x => classes.zip(x).maxBy(_._2)._1 == outputs.predictedValue)
                classes.zip(contributions.transpose.map(_.sum / contributions.size)).toMap
              }
              case `median`          => {
                val sortedMatrix = matrix.map(_.sorted)
                if (selections.size % 2 == 0) {
                  classes.zip(matrix.map(x => x(selections.size / 2))).toMap
                } else {
                  classes.zip(matrix.map(x => x(selections.size / 2) + x(selections.size / 2 - 1) / 2.0)).toMap
                }
              }
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
      for (segment <- segmentation.get.segments) {
        val f = segment.model.output.flatMap(_.getField(name))
        if (f.isDefined) return f
      }
    }

    super.getField(name)
  }

  /** Creates an object of MiningOutputs that is for writing into an output series.  */
  override def createOutputs(): MiningOutputs = new MiningOutputs

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override lazy val defaultOutputFields: Array[OutputField] = {
    if (isSegmentation) {
      import MultipleModelMethod._
      segmentation.get.multipleModelMethod match {
        case `modelChain` if segmentation.get.segments.last.predicate == True => {
          segmentation.get.segments.last.model.candidateOutputFields
        }
        case `selectAll`                                                      => {
          segmentation.get.segments.map(_.model.candidateOutputFields).flatten
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
   * For regression models only average, weightedAverage, median, sum, modelChain, selectFirst, or selectAll are applicable.
   * The first four methods are applied to the predicted values of all models for which the predicate evaluates to true.
   *
   * For classification models all the combination methods, except for sum, can be used. For the first six combination methods
   * the models in all segments must have the same target variable. Note that average, weightedAverage, median, and max are
   * applied to the predicted probabilities of target categories in each of the models used for the case, then the winning
   * category is selected based on the highest combined probability, while majorityVote and weightedMajorityVote use the
   * predicted categories from all applicable models and select the one based on the models' "votes". Predicted probabilities
   * for the final prediction of a classification MiningModel are defined as follows, depending on the combination method:
   * majorityVote and weightedMajorityVote: the probabilities are computed as the proportions of the votes or weighted votes for each target category;
   * average, weightedAverage: the probabilities are computed as the average or weighted average of probabilities of the models used in the prediction;
   * max: consider the model(s) that have contributed the chosen probability for the winning category. Return their average probabilities;
   * median: if the number of models with predicates that resolve to true is odd consider the model(s) that have the chose
   */
  val majorityVote, weightedMajorityVote, average, weightedAverage, median, max, sum, selectFirst, selectAll, modelChain = Value

  def support(mmm: MultipleModelMethod, mf: MiningFunction): Boolean = mmm match {
    case `selectFirst` | `selectAll` | `modelChain` => true
    case `majorityVote` | `weightedMajorityVote`
      if mf == MiningFunction.clustering            => true
    case `average` | `weightedAverage` | `median` | `sum`
      if mf == MiningFunction.regression            => true
    case `majorityVote` | `weightedMajorityVote` | `average` | `weightedAverage` | `median` | `max`
      if mf == MiningFunction.classification        => true
    case _                                          => false
  }
}

class Segmentation(val multipleModelMethod: MultipleModelMethod, val segments: Array[Segment]) extends PmmlElement {

  def +=(segment: Segment): Segmentation = {
    new Segmentation(multipleModelMethod, segments :+ segment)
  }

  def setParent(parent: Model): Segmentation = {
    segments.foreach(x => x.model.setParent(parent))
    this
  }
}

class Segment(val predicate: Predicate, val model: Model, val id: Option[String] = None, val weight: Double = 1.0)
  extends Predictable with Predicate with PmmlElement {

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = model.predict(values)

  /** Evaluates the predicate. */
  override def eval(series: Series): Predication = predicate.eval(series)
}

class MiningOutputs extends ClsOutputs with RegOutputs with CluOutputs with SegmentOutputs
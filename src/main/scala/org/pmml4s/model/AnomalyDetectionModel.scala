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
import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata._
import org.pmml4s.model.AlgorithmType.AlgorithmType
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.Utils

import scala.collection.immutable

/**
 * Anomaly detection (also outlier detection) is the identification of items, events or observations which do not
 * conform to an expected pattern or other items in a data set. Traditional approaches comprise of distance and
 * density-based approaches. Examples of common ways to define distance or density are distance to the k-nearest
 * neighbors or count of points within a given fixed radius. These methods however are unable to handle data sets with
 * regions of different densities and do not scale well for large data. Other algorithms have been proposed which are
 * better able to handle such cases; the PMML standard at this time supports three such algorithms:
 *
 * - Isolation Forest
 * - One Class SVM
 * - Clustering mean distance based anomaly detection model
 * - Other models can also be used if their scoring follows PMML standard rules.
 */
class AnomalyDetectionModel(
                             var parent: Model,
                             override val attributes: AnomalyDetectionAttributes,
                             override val miningSchema: MiningSchema,
                             val model: Model,
                             val meanClusterDistances: Option[MeanClusterDistances] = None,
                             override val output: Option[Output] = None,
                             override val localTransformations: Option[LocalTransformations] = None,
                             override val modelVerification: Option[ModelVerification] = None,
                             override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedAnomalyDetectionAttributes {
  require(algorithmType != AlgorithmType.iforest || sampleDataSize.isDefined,
    "sampleDataSize is a required parameter for isolation forest models.")

  require(algorithmType != AlgorithmType.clusterMeanDist || meanClusterDistances.isDefined,
    "MeanClusterDistances is required when the algorithm type is clusterMeanDist.")

  private val (predictedIndex, affinityIndex) = if (algorithmType == AlgorithmType.clusterMeanDist) {
    var predictedIndex = model.outputIndex(ResultFeature.predictedValue)
    if (predictedIndex == -1) {
      predictedIndex = model.outputIndex(ResultFeature.clusterId)
    }
    if (predictedIndex == -1) {
      predictedIndex = model.outputIndex(ResultFeature.entityId)
    }

    var affinityIndex = model.outputIndex(ResultFeature.affinity)
    if (affinityIndex == -1) {
      affinityIndex = model.outputIndex(ResultFeature.clusterAffinity)
    }
    if (affinityIndex == -1) {
      affinityIndex = model.outputIndex(ResultFeature.entityAffinity)
    }

    (predictedIndex, affinityIndex)
  } else (model.outputIndex(ResultFeature.predictedValue), -1)

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.AnomalyDetectionModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs()

    import AlgorithmType._
    val outSeries = model.predict(series)
    val predictedValue = outSeries.get(predictedIndex)
    if (!Utils.isMissing(predictedValue)) {
      outputs.predictedValue = algorithmType match {
        case `iforest`         => {
          // This normalization is defined as 2^-(predictedValue/c(n)) where n is the sampleDataSize and
          // c(n) = 2*H(n-1) - (2*(n-1)/n). H(x) may be reasonably approximated as ln(x) + 0.57721566 (Eulers constant)
          val n = sampleDataSize.get
          val cn = 2.0 * (Math.log(n - 1.0) + 0.57721566) - (2.0 * (n - 1.0) / n)
          val b = -(Utils.toDouble(predictedValue) / cn)
          DataVal.from(Math.pow(2.0, b))
        }
        case `ocsvm`           => predictedValue
        case `clusterMeanDist` => {
          val clusteringModel = model.asInstanceOf[ClusteringModel]
          val winner = predictedValue
          val winnerIdx = clusteringModel.clusters.zipWithIndex.find(x =>
            if (x._1.id.isDefined) x._1.id.get == winner else DataVal.from((x._2 + 1).toString) == winner
          ).get._2
          val affinity = Utils.toDouble(outSeries.get(affinityIndex))
          DataVal.from(affinity / meanClusterDistances.get.array(winnerIdx))
        }
        case `other`           => ???
      }
    }

    result(series, outputs)
  }

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override def defaultOutputFields: Array[OutputField] = {
    Array(OutputField.predictedValue("anomalyScore", "Anomaly score of detection model", DataType.double, OpType.continuous))
  }

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): AnomalyDetectionOutput = new AnomalyDetectionOutput

  override def targets: Option[Targets] = None

  override def modelStats: Option[ModelStats] = None

  override def modelExplanation: Option[ModelExplanation] = None
}

/**
 * Contains an array of non-negative real values, it is required when the algorithm type is clusterMeanDist. The length
 * of the array must equal the number of clusters in the model, and the values in it are the mean distances/similarities
 * to the center for each cluster.
 *
 * @param array
 */
class MeanClusterDistances(val array: Array[Double]) extends PmmlElement


/**
 * Defines model types used by the anomaly model.
 */
object AlgorithmType extends Enumeration {
  type AlgorithmType = Value

  /**
   * - iforest indicates an isolation forest model which uses a MiningModel element.
   * - ocsvm indicates a one-class SVM model which corresponds to a SupportVectorMachineModel element.
   * - clusterMeanDist indicates a clustering mean distance based anomaly detection model.
   * - other stands for any other model.
   */
  val iforest, ocsvm, clusterMeanDist, other = Value
}

trait HasAnomalyDetectionAttributes extends HasModelAttributes {
  /**
   * Defines a model type used by the anomaly model.
   */
  def algorithmType: AlgorithmType

  /**
   * A required parameter for isolation forest models. It is the dataset size used to train the forest and is needed to
   * normalize the tree search depth.
   */
  def sampleDataSize: Option[Long]
}


/**
 * Holds attributes of an Anomaly Detection Model.
 */
class AnomalyDetectionAttributes(
                                  override val functionName: MiningFunction,
                                  val algorithmType: AlgorithmType,
                                  val sampleDataSize: Option[Long] = None,
                                  override val modelName: Option[String] = None,
                                  override val algorithmName: Option[String] = None,
                                  override val isScorable: Boolean = true)
  extends ModelAttributes(functionName, modelName, algorithmName, isScorable) with HasAnomalyDetectionAttributes

trait HasWrappedAnomalyDetectionAttributes extends HasWrappedModelAttributes with HasAnomalyDetectionAttributes {

  override def attributes: AnomalyDetectionAttributes

  def algorithmType: AlgorithmType = attributes.algorithmType

  def sampleDataSize: Option[Long] = attributes.sampleDataSize
}

class AnomalyDetectionOutput extends RegOutputs {
  override def modelElement: ModelElement = ModelElement.AnomalyDetectionModel
}

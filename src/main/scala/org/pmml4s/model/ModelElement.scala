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
package org.pmml4s.model

sealed trait ModelElement

object ModelElement {

  /**
   * Defines const variables could be used in Java.
   * Use Object.XXX() instead of Object.xxx$.MODULE$
   */
  val DATA_MODEL = DataModel
  val TRANSFORMATION_MODEL = TransformationModel
  val ANOMALY_DETECTION_MODEL = AnomalyDetectionModel
  val ASSOCIATION_MODEL = AssociationModel
  val BAYESIAN_NETWORK_MODEL = BayesianNetworkModel
  val BASELINE_MODEL = BaselineModel
  val CLUSTERING_MODEL = ClusteringModel
  val GAUSSIAN_PROCESS_MODEL = GaussianProcessModel
  val GENERAL_REGRESSION_MODEL = GeneralRegressionModel
  val MINING_MODEL = MiningModel
  val NAIVE_BAYES_MODEL = NaiveBayesModel
  val NEAREST_NEIGHBOR_MODEL = NearestNeighborModel
  val NEURAL_NETWORK = NeuralNetwork
  val REGRESSION_MODEL = RegressionModel
  val RULE_SET_MODEL = RuleSetModel
  val SEQUENCE_MODEL = SequenceModel
  val SCORECARD = Scorecard
  val SUPPORT_VECTOR_MACHINE_MODEL = SupportVectorMachineModel
  val TEXT_MODEL = TextModel
  val TIME_SERIES_MODEL = TimeSeriesModel
  val TREE_MODEL = TreeModel
  val DECISION_TREE = DecisionTree
  val REGRESSION = Regression

  case object DataModel extends ModelElement

  case object TransformationModel extends ModelElement

  case object AnomalyDetectionModel extends ModelElement

  case object AssociationModel extends ModelElement

  case object BayesianNetworkModel extends ModelElement

  case object BaselineModel extends ModelElement

  case object ClusteringModel extends ModelElement

  case object GaussianProcessModel extends ModelElement

  case object GeneralRegressionModel extends ModelElement

  case object MiningModel extends ModelElement

  case object NaiveBayesModel extends ModelElement

  case object NearestNeighborModel extends ModelElement

  case object NeuralNetwork extends ModelElement

  case object RegressionModel extends ModelElement

  case object RuleSetModel extends ModelElement

  case object SequenceModel extends ModelElement

  case object Scorecard extends ModelElement

  case object SupportVectorMachineModel extends ModelElement

  case object TextModel extends ModelElement

  case object TimeSeriesModel extends ModelElement

  case object TreeModel extends ModelElement

  case object DecisionTree extends ModelElement

  case object Regression extends ModelElement

}

trait ModelLocation {
  self: Model =>

  val isTopLevelModel: Boolean = if (parent != null) parent.modelElement == ModelElement.DataModel else true

  val isSubModel: Boolean = !isTopLevelModel
}

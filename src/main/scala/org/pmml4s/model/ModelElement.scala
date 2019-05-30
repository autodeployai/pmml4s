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

sealed trait ModelElement

object ModelElement {

  case object DataModel extends ModelElement

  case object TransformationModel extends ModelElement

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

  def isTopLevelModel: Boolean = if (parent != null) parent.modelElement == ModelElement.DataModel else true

  def isSubModel: Boolean = !isTopLevelModel
}
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

import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata.{MiningSchema, Output, Targets}
import org.pmml4s.transformations.LocalTransformations

/**
 * Model Composition
 */
abstract class EmbeddedModel extends Model {

  override def modelVerification: Option[ModelVerification] = None

  override def createOutputs(): ModelOutputs = ???

  override def output: Option[Output] = ???

  override def miningSchema: MiningSchema = null

  override def modelExplanation: Option[ModelExplanation] = None

  override def modelStats: Option[ModelStats] = ???

  override def targets: Option[Targets] = ???

  override def localTransformations: Option[LocalTransformations] = ???

  override def attributes: ModelAttributes = ???
}

class Regression(var parent: Model) extends EmbeddedModel {

  ???

  override def modelElement: ModelElement = ModelElement.Regression

  override def predict(values: Series): Series = ???
}

class DecisionTree(var parent: Model) extends EmbeddedModel {
  ???

  override def modelElement: ModelElement = ModelElement.DecisionTree

  override def predict(values: Series): Series = ???
}

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

import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata.{MiningSchema, Output, Targets}
import org.pmml4s.transformations.LocalTransformations

class MutableModel extends Model {

  var parent: Model = _

  /** The optional local transformations. */
  var localTransformations: Option[LocalTransformations] = None

  /** Model element type. */
  var modelElement: ModelElement = _

  /** Common attributes of this model */
  var attributes: ModelAttributes = _

  var targets: Option[Targets] = None

  var output: Option[Output] = None

  var miningSchema: MiningSchema = _

  var modelVerification: Option[ModelVerification] = None

  var modelExplanation: Option[ModelExplanation] = None

  var modelStats: Option[ModelStats] = None

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = ???

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): ModelOutputs = ???
}
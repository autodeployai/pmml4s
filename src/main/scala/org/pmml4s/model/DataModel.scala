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

import org.pmml4s.common.MiningFunction._
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata._
import org.pmml4s.transformations.{LocalTransformations, TransformationDictionary}

/**
 * DataModel is a container for all info about metadata, it's the parent model of all predictive models.
 */
class DataModel(
                 override val version: String,
                 override val header: Header,
                 override val dataDictionary: DataDictionary,
                 override val transformationDictionary: Option[TransformationDictionary] = None) extends Model {

  var parent: Model = _

  override def modelElement: ModelElement = ModelElement.DataModel

  override def getField(name: String): Option[Field] = dataDictionary.get(name) orElse
    (transformationDictionary.flatMap { x => x.get(name) })

  override def predict(values: Series): Series = transformationDictionary.map(_.transform(values)).getOrElse(values)

  override def miningSchema: MiningSchema = null

  override def output: Option[Output] = None

  override def modelVerification: Option[ModelVerification] = None

  override def modelExplanation: Option[ModelExplanation] = None

  override def modelStats: Option[ModelStats] = None

  override def targets: Option[Targets] = None

  override def localTransformations: Option[LocalTransformations] = None

  override def attributes: ModelAttributes = null

  override def modelName: Option[String] = None

  override def functionName: MiningFunction = null

  override def algorithmName: Option[String] = None

  override def isScorable: Boolean = false

  override def defaultOutputFields: Array[OutputField] = Array.empty

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): ModelOutputs = new ModelOutputs {}

  def asTransformation: TransformationModel =
    new TransformationModel(version, header, dataDictionary, transformationDictionary)
}

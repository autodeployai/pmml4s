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

import org.pmml4s.common.{Header, ModelOutputs}
import org.pmml4s.data.{CombinedSeries, Series}
import org.pmml4s.metadata.{DataDictionary, OutputField, ResultFeature}
import org.pmml4s.transformations.TransformationDictionary

class TransformationModel(
                           override val version: String,
                           override val header: Header,
                           override val dataDictionary: DataDictionary,
                           override val transformationDictionary: Option[TransformationDictionary])
  extends DataModel(version, header, dataDictionary, transformationDictionary) {

  require(transformationDictionary.nonEmpty, "TransformationDictionary is required for a transformation model")

  override lazy val inputNames: Array[String] = dataDictionary.map(_.name).toArray

  override def modelElement: ModelElement = ModelElement.TransformationModel

  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val res = transformationDictionary.get.transform(series)
    res.asInstanceOf[CombinedSeries].individualRows.last
  }

  override def defaultOutputFields: Array[OutputField] = {
    transformationDictionary.get.map(x =>
      new OutputField(x.name, x.displayName, x.dataType, x.opType, ResultFeature.transformedValue, expr=Some(x.expr))).toArray
  }

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): ModelOutputs = new ModelOutputs() {}
}

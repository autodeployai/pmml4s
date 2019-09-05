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
package org.pmml4s.xml

import org.pmml4s.NotSupportedException
import org.pmml4s.model._
import org.pmml4s.xml.XmlImplicits._

import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Builder of Anomaly Detection Model
 */
class AnomalyDetectionBuilder extends Builder[AnomalyDetectionModel] {
  protected var attributes: AnomalyDetectionAttributes = _
  private var model: Model = _
  private var meanClusterDistances: MeanClusterDistances = _

  /** Builds an Anomaly Detection model from a specified XML reader, which points the element <AnomalyDetectionModel> */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): AnomalyDetectionModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.ANOMALY_DETECTION_MODEL, {
      case EvElemStart(_, label, attrs, _) if ModelBuilder.PMML_SUPPORTED_MODELS contains label => {
        val builder = Builder.get(label).getOrElse(throw new NotSupportedException(label))
        model = builder.build(reader, attrs, new MutableAnomalyDetectionModel(parent))
        builder.postBuild()
      }
      case EvElemStart(_, ElemTags.MEAN_CLUSTER_DISTANCES, attrs, _)                            => {
        meanClusterDistances = makeMeanClusterDistances(reader, attrs)
      }
    })

    val result = new AnomalyDetectionModel(parent, attributes, miningSchema,
      model, Option(meanClusterDistances),
      output, localTransformations, modelVerification, extensions.toIndexedSeq)
    model.parent = result

    result
  }

  def makeMeanClusterDistances(reader: XMLEventReader, attrs: MetaData): MeanClusterDistances = makeElem(reader, attrs,
    new ElemBuilder[MeanClusterDistances] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): MeanClusterDistances = {
        var array: Array[Double] = null
        traverseElems(reader, ElemTags.MEAN_CLUSTER_DISTANCES, {
          case EvElemStart(_, ElemTags.ARRAY, attrs, _) => array = makeRealArray(reader, attrs)
        })
        new MeanClusterDistances(array)
      }
    })

  /** Extracts these common attributes with model specific from a model */
  override protected def makeAttributes(attrs: XmlAttrs): AnomalyDetectionAttributes = {
    val attributes = super.makeAttributes(attrs)
    new AnomalyDetectionAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      algorithmType = AlgorithmType.withName(attrs(AttrTags.ALGORITHM_TYPE)),
      sampleDataSize = attrs.getLong(AttrTags.SAMPLE_DATA_SIZE))
  }

  /** Name of the builder. */
  override def name: String = ElemTags.ANOMALY_DETECTION_MODEL
}

class MutableAnomalyDetectionModel extends MutableModel {
  def this(parent: Model) {
    this()
    this.parent = parent
  }
}

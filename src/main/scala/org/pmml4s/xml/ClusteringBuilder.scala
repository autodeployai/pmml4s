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
package org.pmml4s.xml

import org.pmml4s.common.{CompareFunction, ComparisonMeasure, Matrix, Partition}
import org.pmml4s.data.DataVal
import org.pmml4s.model._

import scala.collection.mutable

/**
 * Builder of Clustering model
 */
class ClusteringBuilder extends Builder[ClusteringModel] {
  protected var attributes: ClusteringAttributes = _
  private var comparisonMeasure: ComparisonMeasure = _
  private val clusteringFields = mutable.ArrayBuilder.make[ClusteringField]
  private var missingValueWeights: MissingValueWeights = _
  private val clusters = mutable.ArrayBuilder.make[Cluster]

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): ClusteringModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)
    clusters.sizeHint(attributes.numberOfClusters)

    traverseModel(reader, ElemTags.CLUSTERING_MODEL, {
      case EvElemStart(_, ElemTags.COMPARISON_MEASURE, attrs, _) => comparisonMeasure = makeComparisonMeasure(reader, attrs)
      case EvElemStart(_, ElemTags.CLUSTERING_FIELD, attrs, _)   => clusteringFields += makeClusteringField(reader, attrs)
      case EvElemStart(_, ElemTags.MISSING_VALUE_WEIGHTS, _, _)  => missingValueWeights = makeMissingValueWeights(reader)
      case EvElemStart(_, ElemTags.CLUSTER, attrs, _)            => clusters += makeCluster(reader, attrs)
    })

    new ClusteringModel(parent, attributes, miningSchema,
      comparisonMeasure, clusteringFields.result(), Option(missingValueWeights), clusters.result(),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeCluster(reader: XMLEventReader, attrs: XmlAttrs): Cluster = makeElem(reader, attrs, new ElemBuilder[Cluster] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): Cluster = {
      val id = attrs.get(AttrTags.ID).map(DataVal.from)
      val name = attrs.get(AttrTags.NAME)
      val size = attrs.getInt(AttrTags.SIZE)
      var kohonenMap: KohonenMap = null
      var array: Array[Double] = null
      var partition: Partition = null
      var covariances: Covariances = null
      traverseElems(reader, ElemTags.CLUSTER, {
        case EvElemStart(_, ElemTags.KOHONEN_MAP, attrs, _) => kohonenMap = makeElem(reader, attrs, new ElemBuilder[KohonenMap] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): KohonenMap = {
            val coord1 = attrs.getDouble(AttrTags.COORD1)
            val coord2 = attrs.getDouble(AttrTags.COORD2)
            val coord3 = attrs.getDouble(AttrTags.COORD3)
            new KohonenMap(coord1, coord2, coord3)
          }
        })
        case EvElemStart(_, ElemTags.ARRAY, attrs, _)       => array = makeRealArray(reader, attrs)
        case EvElemStart(_, ElemTags.PARTITION, attrs, _)   => partition = makePartition(reader, attrs)
        case EvElemStart(_, ElemTags.COVARIANCES, attrs, _) => covariances = makeElem(reader, attrs, new ElemBuilder[Covariances] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): Covariances = {
            val matrix = makeElem(reader, ElemTags.COVARIANCES, ElemTags.MATRIX, new ElemBuilder[Matrix] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): Matrix = makeMatrix(reader, attrs)
            })
            new Covariances(matrix.get)
          }
        })
      })

      new Cluster(id, name, size, Option(kohonenMap), Option(array), Option(partition), Option(covariances))
    }
  })

  def makeMissingValueWeights(reader: XMLEventReader): MissingValueWeights = {
    val array = makeElem(reader, ElemTags.MISSING_VALUE_WEIGHTS, ElemTags.ARRAY, new ElemBuilder[Array[Double]] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Array[Double] = makeRealArray(reader, attrs)
    })

    new MissingValueWeights(array.get)
  }

  def makeClusteringField(reader: XMLEventReader, attrs: XmlAttrs): ClusteringField = makeElem(reader, attrs, new ElemBuilder[ClusteringField] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): ClusteringField = {
      val f = field(attrs(AttrTags.FIELD))
      val isCenterField = attrs.getBoolean(AttrTags.IS_CENTER_FIELD, true)
      val fieldWeight = attrs.getDouble(AttrTags.FIELD_WEIGHT, 1.0)
      val similarityScale = attrs.getDouble(AttrTags.SIMILARITY_SCALE)
      val compareFunction = attrs.get(AttrTags.COMPARE_FUNCTION).map(CompareFunction.withName(_))
      val comparisons = makeElem(reader, ElemTags.CLUSTERING_FIELD, ElemTags.COMPARISONS, new ElemBuilder[Comparisons] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Comparisons = {
          val matrix = makeElem(reader, ElemTags.COVARIANCES, ElemTags.MATRIX, new ElemBuilder[Matrix] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): Matrix = makeMatrix(reader, attrs)
          })
          new Comparisons(matrix.get)
        }
      })

      new ClusteringField(f, comparisons, isCenterField, fieldWeight, similarityScale, compareFunction)
    }
  })

  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): ClusteringAttributes = {
    val attributes = super.makeAttributes(attrs)
    new ClusteringAttributes(attributes,
      modelClass = ModelClass.withName(attrs(AttrTags.MODEL_CLASS)),
      numberOfClusters = attrs.int(AttrTags.NUMBER_OF_CLUSTERS)
    )
  }

  /** Name of the builder. */
  override def name: String = ElemTags.CLUSTERING_MODEL
}


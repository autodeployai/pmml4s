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

import org.pmml4s.common.CompareFunction.CompareFunction
import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata._
import org.pmml4s.model.ModelClass.ModelClass
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.Utils

import scala.collection.{immutable, mutable}

/**
 * A cluster model basically consists of a set of clusters. For each cluster a center vector can be given. In
 * center-based models a cluster is defined by a vector of center coordinates. Some distance measure is used to
 * determine the nearest center, that is the nearest cluster for a given input record. For distribution-based models
 * (e.g., in demographic clustering) the clusters are defined by their statistics. Some similarity measure is used to
 * determine the best matching cluster for a given record. The center vectors then only approximate the clusters.
 */
class ClusteringModel(
                       override var parent: Model,
                       override val attributes: ClusteringAttributes,
                       override val miningSchema: MiningSchema,
                       val comparisonMeasure: ComparisonMeasure,
                       val clusteringFields: Array[ClusteringField],
                       val missingValueWeights: Option[MissingValueWeights],
                       val clusters: Array[Cluster],
                       override val output: Option[Output] = None,
                       override val targets: Option[Targets] = None,
                       override val localTransformations: Option[LocalTransformations] = None,
                       override val modelStats: Option[ModelStats] = None,
                       override val modelExplanation: Option[ModelExplanation] = None,
                       override val modelVerification: Option[ModelVerification] = None,
                       override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedClusteringAttributes {

  // A vector of field weight values, Wi, i=1,...,n
  private val weights: Array[Double] = clusteringFields.map(_.fieldWeight)
  private val compareFunctions: Array[CompareFunction] =
    clusteringFields.map(_.compareFunction.getOrElse(comparisonMeasure.compareFunction))
  private val similarityScales: Array[Double] = clusteringFields.map(_.similarityScale.getOrElse(1.0))

  // A vector of adjustment values, Qi, i=1,...,n, all nonmissing Qi is the i-th value in the element
  // MissingValueWeights. If the model does not have MissingValueWeights, Qi is assumed to be 1.0.
  private val Qi: Array[Double] = missingValueWeights.map((_.array)).getOrElse(Array.fill(clusteringFields.length)(1.0))
  private val sumQi = Qi.sum
  private val dis = comparisonMeasure.distance

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.ClusteringModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    var nonMissing: Double = 0.0
    val nonMissingIdxBuilder = mutable.ArrayBuilder.make[Int]
    nonMissingIdxBuilder.sizeHint(clusteringFields.length)
    val xs = new Array[Double](clusteringFields.length)
    var i = 0
    while (i < clusteringFields.length) {
      xs(i) = clusteringFields(i).field.getDouble(series)
      if (Utils.nonMissing(xs(i))) {
        nonMissing += Qi(i)
        nonMissingIdxBuilder += i
      }
      i += 1
    }
    val nonMissingIdx = nonMissingIdxBuilder.result()

    if (nonMissing == 0) {
      return nullSeries
    }

    // The adjustment values are used to compute an adjustment factor
    //
    //                      sum[Qi]
    // AdjustM   =  --------------------------
    //                sum[nonmissing(Xi)*Qi]
    //
    val adjustM = sumQi / nonMissing
    val (id, name, affinities) = comparisonMeasure.kind match {
      case ComparisonMeasureKind.distance   => {
        var min = Double.PositiveInfinity
        var selected: String = null
        var name: Option[String] = None
        val distances = mutable.Map.empty[String, Double]
        var i = 0
        while (i < clusters.length) {
          val distance = dis.distance(nonMissingIdx, compareFunctions, xs, clusters(i).array.get, weights,
            adjustM, similarityScales)
          val id = clusters(i).id.getOrElse("" + (i + 1))
          if (distance < min) {
            min = distance
            selected = id
            name = clusters(i).name
          }
          distances.put(id, distance)
          i += 1
        }
        (selected, name, distances)
      }
      case ComparisonMeasureKind.similarity => {
        var max = Double.NegativeInfinity
        var selected: String = null
        var name: Option[String] = None
        val similarities = mutable.Map.empty[String, Double]
        var i = 0
        while (i < clusters.length) {
          val similarity = dis.distance(nonMissingIdx, compareFunctions, xs, clusters(i).array.get, weights,
            adjustM, similarityScales)
          val id = clusters(i).id.getOrElse("" + (i + 1))
          if (similarity > max) {
            max = similarity
            selected = id
            name = clusters(i).name
          }
          similarities.put(id, similarity)
          i += 1
        }
        (selected, name, similarities)
      }
    }

    val outputs = createOutputs().
      setPredictedValue(id).
      setPredictedDisplayValue(name.orNull).
      setEntityId(id).
      setAffinities(affinities.toMap)

    result(series, outputs)
  }

  /** Creates an object of ClusteringOutputs that is for writing into an output series.  */
  override def createOutputs(): ClusteringOutputs = new ClusteringOutputs

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override def defaultOutputFields: Array[OutputField] = {
    val res = mutable.ArrayBuilder.make[OutputField]
    res.sizeHint(3)
    res += OutputField.predictedValue("cluster", "Identifier of the winning cluster", StringType, OpType.nominal)

    if (clusters.head.name.isDefined) {
      res += OutputField.predictedDisplayValue("cluster_name", "Name of the winning cluster")
    }

    res += (if (modelClass == ModelClass.centerBased) {
      OutputField.affinity("distance", "Distance to the predicted entity")
    } else {
      OutputField.affinity("similarity", "Similarity to the predicted entity")
    })

    res.result()
  }
}

/**
 * @param field           Refers (by name) to a MiningField or to a DerivedField.
 * @param comparisons     A matrix which contains the similarity values or distance values.
 * @param isCenterField   Indicates whether the respective field is a center field, i.e. a component of the center, in a
 *                        center-based model. Only center fields correspond to the entries in the center vectors in order.
 * @param fieldWeight     The importance factor for the field. This field weight is used in the comparison functions in
 *                        order to compute the comparison measure. The value must be a number greater than 0. The default
 *                        value is 1.0.
 * @param similarityScale The distance such that similarity becomes 0.5.
 * @param compareFunction A function of taking two field values and a similarityScale to define similarity/distance.
 *                        It can override the general specification of compareFunction in ComparisonMeasure.
 */
class ClusteringField(val field: Field,
                      val comparisons: Option[Comparisons],
                      val isCenterField: Boolean = true,
                      val fieldWeight: Double = 1.0,
                      val similarityScale: Option[Double] = None,
                      val compareFunction: Option[CompareFunction] = None
                     ) extends PmmlElement

/**
 * Comparisons is a matrix which contains the similarity values or distance values, depending on the attribute
 * modelClass in ClusteringModel. The order of the rows and columns corresponds to the order of discrete values or
 * intervals in that field.
 */
class Comparisons(val matrix: Matrix) extends PmmlElement

/**
 * A cluster is defined by its center vector or by statistics. A center vector is implemented by a NUM-ARRAY. Each
 * Partition corresponds to a cluster and holds field statistics to describe it. The definition of a cluster may
 * contain a center vector as well as statistics. The attribute modelClass in the ClusteringModel defines which one is
 * used to actually define the cluster.
 */
class Cluster(val id: Option[String] = None,
              val name: Option[String] = None,
              val size: Option[Int] = None,
              val kohonenMap: Option[KohonenMap] = None,
              val array: Option[Array[Double]] = None,
              val partition: Option[Partition] = None,
              val covariances: Option[Covariances] = None) extends PmmlElement

/**
 * The element KohonenMap is appropriate for clustering models that were produced by a Kohonen map algorithm. The
 * attributes coord1, coord2 and coord3 describe the position of the current cluster in a map with up to three
 * dimensions. This element is not relevant to the scoring function.
 */
class KohonenMap(val coord1: Option[Double], val coord2: Option[Double], val coord3: Option[Double]) extends PmmlElement

/**
 * Stores coordinate-by-coordinate variances (diagonal cells) and covariances (non-diagonal cells).
 */
class Covariances(val matrix: Matrix) extends PmmlElement

/**
 * MissingValueWeights is used to adjust distance or similarity measures for missing data.
 */
class MissingValueWeights(val array: Array[Double]) extends PmmlElement

object ModelClass extends Enumeration {
  type ModelClass = Value
  val centerBased, distributionBased = Value
}

trait HasClusteringAttributes extends HasModelAttributes {

  /**
   * Specifies whether the clusters are defined by center-vectors or whether they are defined by the statistics. The
   * latter is used by distribution-based clustering.
   */
  def modelClass: ModelClass

  /**
   * The numberOfClusters attribute must be equal to the number of Cluster elements in the ClusteringModel.
   */
  def numberOfClusters: Int
}

trait HasWrappedClusteringAttributes extends HasWrappedModelAttributes with HasClusteringAttributes {

  override def attributes: ClusteringAttributes

  def modelClass: ModelClass = attributes.modelClass

  def numberOfClusters: Int = attributes.numberOfClusters
}

class ClusteringAttributes(
                            val modelClass: ModelClass,
                            val numberOfClusters: Int,
                            override val functionName: MiningFunction,
                            override val modelName: Option[String] = None,
                            override val algorithmName: Option[String] = None,
                            override val isScorable: Boolean = true
                          ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable)
  with HasClusteringAttributes {
  def this(attributes: ModelAttributes, modelClass: ModelClass, numberOfClusters: Int) = {
    this(modelClass, numberOfClusters, attributes.functionName, attributes.modelName, attributes.algorithmName, attributes.isScorable)
  }
}

class ClusteringOutputs extends CluOutputs

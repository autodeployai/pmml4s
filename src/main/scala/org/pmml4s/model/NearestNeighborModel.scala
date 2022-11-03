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
import org.pmml4s.model.CatScoringMethod.CatScoringMethod
import org.pmml4s.model.ContScoringMethod.ContScoringMethod
import org.pmml4s.transformations.{LocalTransformations, Median}
import org.pmml4s.util.Utils

import scala.collection.immutable

/**
 * k-Nearest Neighbors (k-NN) is an instance-based learning algorithm. In a k-NN model, a hypothesis or generalization
 * is built from the training data directly at the time a query is made to the system. The prediction is based on the K
 * training instances closest to the case being scored. Therefore, all training cases have to be stored, which may be
 * problematic when the amount of data is large. This model has the ability to store the data directly in PMML using
 * InlineTable or elsewhere using the TableLocator element defined in the Taxonomy document.
 *
 * A k-NN model can have one or more target variables or no targets. When one or more targets are present, the predicted
 * value is computed based on the target values of the nearest neighbors. When no targets are present, the model
 * specifies a case ID variable for the training data. In this way, one can easily obtain the IDs of the K closest
 * training cases (nearest neighbors).
 *
 * A k-NN model consists of four major parts:
 *
 * - Model attributes
 * - Training instances
 * - Comparison measure
 * - Input fields
 */
class NearestNeighborModel(
                            override var parent: Model,
                            override val attributes: NearestNeighborAttributes,
                            override val miningSchema: MiningSchema,
                            val trainingInstances: TrainingInstances,
                            val comparisonMeasure: ComparisonMeasure,
                            val knnInputs: KNNInputs,
                            override val output: Option[Output] = None,
                            override val targets: Option[Targets] = None,
                            override val localTransformations: Option[LocalTransformations] = None,
                            override val modelStats: Option[ModelStats] = None,
                            override val modelExplanation: Option[ModelExplanation] = None,
                            override val modelVerification: Option[ModelVerification] = None,
                            override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedNearestNeighborAttributes {

  require(hasTarget || instanceIdVariable.isDefined, "The instance ID variable is required if the model has no targets.")

  trainingInstances.init(this)

  // A vector of field weight values, Wi, i=1,...,n
  private val weights: Array[Double] = knnInputs.weights
  private val compareFunctions: Array[CompareFunction] =
    knnInputs.knnInputs.map(_.compareFunction.getOrElse(comparisonMeasure.compareFunction))
  private val dis = comparisonMeasure.distance

  // Transformed training instances
  private lazy val matrix: Array[Array[Double]] = {
    val res = Array.ofDim[Double](trainingInstances.nbRows, knnInputs.size)

    // Check if there are derived fields in KNN inputs that not in training instances
    if (!knnInputs.containsDeriveField(trainingInstances.names.toSet)) {
      val columns = trainingInstances.columns(knnInputs.names)
      var i = 0
      while (i < trainingInstances.nbRows) {
        val values = trainingInstances(i, columns)
        var j = 0
        while (j < values.length) {
          res(i)(j) = Utils.toDouble(values(j))
          j += 1
        }
        i += 1
      }
    } else {
      val fields = knnInputs.fields
      var i = 0
      while (i < trainingInstances.nbRows) {
        val series = trainingInstances.series(i)
        val transformed = parent.predict(series)
        val localTransformed = localTransformations.map(_.transform(transformed)).getOrElse(transformed)
        var j = 0
        while (j < fields.length) {
          res(i)(j) = fields(j).getDouble(localTransformed)
          j += 1
        }
        i += 1
      }
    }

    res
  }

  // In case of a tie, the category with the largest number of cases in the training data is the winner.
  private lazy val numCases4CatTargets: Map[String, Map[Any, Long]] = {
    val catTargets = targetFields.filter(_.isCategorical).map(_.name)
    val distributions: Array[Array[Any]] = Array.ofDim(trainingInstances.nbRows, catTargets.length)
    var i = 0
    while (i < trainingInstances.nbRows) {
      val row = trainingInstances.row(i)
      val cats = catTargets.map(row(_))
      var j = 0
      while (j < catTargets.length) {
        distributions(i)(j) = cats(j)
        j += 1
      }
      i += 1
    }
    catTargets.zipWithIndex.map(x => (x._1, Utils.reduceByKey(distributions(x._2).map(y => (y, 1L))))).toMap
  }

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.NearestNeighborModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val xs = knnInputs.knnInputs.map(x => {
      val v = x.field.get(series)

      // check if it's missing
      if (Utils.isMissing(v)) {
        return nullSeries
      }
      Utils.toDouble(v)
    })

    val nonMissing = Array.range(0, knnInputs.size)
    val distances = Array.ofDim[Double](matrix.length)
    var i = 0
    val s = Array.fill(nonMissing.length)(1.0)
    while (i < matrix.length) {
      distances(i) = dis.distance(nonMissing, compareFunctions, xs, matrix(i), weights, s=s)
      i += 1
    }

    val sorted = distances.zipWithIndex.sortBy(_._1)
    val topK: Array[(Double, Int)] = comparisonMeasure.kind match {
      case ComparisonMeasureKind.distance   => {
        sorted.take(numberOfNeighbors)
      }
      case ComparisonMeasureKind.similarity => {
        sorted.takeRight(numberOfNeighbors).reverse
      }
    }

    // KNN allows one or more target variables or no targets
    val outputs: ModelOutputs = if (hasTarget) {
      if (singleTarget) {
        createOutputsByTarget(topK, targetField)
      } else {
        val out = new GenericMultiModelOutputs
        for (t <- targetFields) {
          out.put(t.name, createOutputsByTarget(topK, t))
        }
        out
      }
    } else {
      val entities = topK.map(k => instanceIdVariable.map(x => trainingInstances.row(k._2)(x)).orNull)
      createOutputs().setEntitiesId(entities.toArray).setAffinities(entities.zip(topK).map(x => (x._1, x._2._1)).toMap)
    }

    result(series, outputs)
  }

  def createOutputsByTarget(topK: Array[(Double, Int)], target: Field): ModelOutputs = {
    val outputs = createOutputs()

    val col = trainingInstances.column(target.name)
    val predictions = topK.map(x => trainingInstances.row(x._2)(col))
    if (target.isCategorical) {
      import CatScoringMethod._
      categoricalScoringMethod match {
        case `majorityVote`         => {
          val votes = Utils.reduceByKey(predictions.map(x => (x, 1L)))
          val max = votes.maxBy(_._2)
          val ties = votes.filter(x => x._2 == max._2)
          outputs.predictedValue = if (ties.size > 1) {
            val cases = ties.map(x => (x._1, numCases4CatTargets(col).getOrElse(x._1, 0L)))
            val maxCases: (Any, Long) = cases.maxBy(_._2)
            val tiesCases: Map[Any, Long] = cases.filter(x => x._2 == maxCases._2)
            if (tiesCases.size > 1) {
              tiesCases.toSeq.sortBy(_._2).head._1
            } else {
              maxCases._1
            }
          } else {
            max._1
          }
        }
        case `weightedMajorityVote` => {
          val ws = topK.map(x => 1.0 / (x._1 + threshold))
          val sum = ws.sum
          val weights = ws.map(_ / sum)
          val probabilities = Utils.reduceByKey(predictions.zip(weights)).map(x => (x._1, x._2 / predictions.size)).toMap.withDefaultValue(0.0)
          outputs.evalPredictedValueByProbabilities(probabilities)
        }
      }
    } else if (target.isContinuous) {
      import ContScoringMethod._
      val contPredictions = predictions.map(Utils.toDouble(_))
      outputs.predictedValue = continuousScoringMethod match {
        case `median`          => {
          Median.eval(contPredictions: _*)
        }
        case `average`         => {
          contPredictions.sum / contPredictions.length
        }
        case `weightedAverage` => {
          val ws = topK.map(x => 1.0 / (x._1 + threshold))
          val sum = ws.sum
          ws.map(_ / sum).zip(contPredictions).map(x => x._1 * x._2).sum / contPredictions.length
        }
      }
    }

    if (instanceIdVariable.isDefined) {
      instanceIdVariable.foreach(x => {
        val entities = topK.map(k => trainingInstances.row(k._2)(x))
        outputs.setEntitiesId(entities.toArray).setAffinities(entities.zip(topK).map(x => (x._1, x._2._1)).toMap)
      })
    } else {
      outputs.setEntitiesId(topK.map(x => x._2 + 1)).setAffinities(topK.map(x => (x._2 + 1, x._1)).toMap)
    }

    outputs
  }

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): NearestNeighborModelOutputs = new NearestNeighborModelOutputs
}

/**
 * Encapsulates the definition of the fields included in the training instances as well as their values.
 *
 * @param instanceFields Defines all the fields included in the training instances.
 * @param table          Representing the training data (feature vectors and class labels)
 * @param isTransformed  Used as a flag to determine whether or not the training instances have already been transformed.
 *                       If isTransformed is "false", it indicates that the training data has not been transformed yet.
 *                       If "true", it indicates that it has already been transformed.
 * @param recordCount    Defines the number of training instances or records. This number needs to match the number of
 *                       instances defined in the element InlineTable or in the external data if TableLocator is used.
 * @param fieldCount     Defines the number of fields (features + targets). This number needs to match the number of
 *                       InstanceField elements defined under InstanceFields.
 */
class TrainingInstances(val instanceFields: InstanceFields,
                        val table: Table,
                        val isTransformed: Boolean = false,
                        val recordCount: Option[Int] = None,
                        val fieldCount: Option[Int] = None) extends PmmlElement {
  val names: Array[String] = instanceFields.names
  val columns: Array[String] = instanceFields.columns
  private var schema: StructType = _

  def init(scope: FieldScope): Unit = {
    schema = new StructType(names.map(x => StructField(x, scope.getField(x).map(_.dataType).getOrElse(StringType))))
  }

  def columns(names: Array[String]): Array[String] = names.map(instanceFields(_))

  def column(name: String): String = instanceFields(name)

  def apply(i: Int, columns: Array[String]): Array[Any] = {
    val row = table(i)
    columns.map(row(_))
  }

  def series(i: Int): Series = {
    val row = table(i)
    Series.fromArray(columns.map(x => row(x)), schema)
  }

  def row(i: Int): Row = {
    table(i)
  }

  def nbRows: Int = table.dim._1

  def nbCols: Int = table.dim._2
}

/**
 * Serves as an envelope for all the fields included in the training instances. It encapsulates InstanceField elements.
 *
 * @param instanceFields
 */
class InstanceFields(val instanceFields: Array[InstanceField]) extends PmmlElement {
  private val fieldToColumn = instanceFields.map(x => (x.field, x.column.get)).toMap

  def apply(name: String): String = fieldToColumn(name)

  def names: Array[String] = instanceFields.map(_.field)

  def columns: Array[String] = instanceFields.map(_.column.get)
}

/**
 *
 * @param field  Contains the name of a DataField or a DerivedField (in case isTransformed is set to "true"). Can also
 *               contain the name of the case ID variable.
 * @param column Defines the name of the tag or column used by element InlineTable. This attribute is required if element
 *               InlineTable is used to represent training data.
 */
class InstanceField(val field: String, val column: Option[String]) extends PmmlElement

/**
 * encapsulates several KNNInput elements which define the fields used to query the k-NN model, one KNNInput element
 * per field.
 *
 * @param knnInputs
 */
class KNNInputs(val knnInputs: Array[KNNInput]) extends PmmlElement {
  def weights: Array[Double] = knnInputs.map(_.fieldWeight)

  def size: Int = knnInputs.length

  def names: Array[String] = knnInputs.map(_.field.name)

  def fields: Array[Field] = knnInputs.map(_.field)

  def containsDeriveField(trainingNames: Set[String]): Boolean = knnInputs.exists(
    x => x.field.isDerivedField && !trainingNames.contains(x.field.name))
}

/**
 *
 * @param field       Contains the name of a DataField or a DerivedField. If a DerivedField is used and isTransformed is
 *                    false, the training instances will also need to be transformed together with the k-NN input.
 * @param compareFunction
 * @param fieldWeight Defines the importance factor for the field. It is used in the comparison functions to compute the
 *                    comparison measure. The value must be a number greater than 0. The default value is 1.0.
 */
class KNNInput(val field: Field, val compareFunction: Option[CompareFunction], val fieldWeight: Double = 1.0) extends PmmlElement


object ContScoringMethod extends Enumeration {
  type ContScoringMethod = Value

  /**
   * - median: continuous targets, predicted target is the median of targets for the k-nearest neighbors.
   * - average: continuous targets, predicted target is the average of targets for the k-nearest neighbors.
   * - weightedAverage: continuous targets, predicted target is the weighted average of targets for the k-nearest
   * neighbors. The weights are proportional to the inverse of the distance from each k-neighbor to the query point.
   */
  val median, average, weightedAverage = Value
}

object CatScoringMethod extends Enumeration {
  type CatScoringMethod = Value

  /**
   * - majorityVote: categorical targets, predicted target corresponds to the category with the highest frequency of
   * occurrence among the k-nearest neighbors. In case of a tie, the category with the largest number of cases in the
   * training data is the winner. If multiple categories are tied on the largest number of cases in the training data,
   * then the category with the smallest data value (in lexical order) among the tied categories is the winner.
   *
   * - weightedMajorityVote: categorical targets, predicted target corresponds to the category with the highest weighted
   * frequency of occurrence among the k-nearest neighbors. The weights are proportional to the inverse of the distance
   * from each k-neighbor to the query point.
   */
  val majorityVote, weightedMajorityVote = Value
}

trait HasNearestNeighborAttributes extends HasModelAttributes {

  /** Specifies K, the number of desired neighbors. */
  def numberOfNeighbors: Int

  /** Specify the scoring (or combining) method based on the continuous target values of K neighbors. */
  def continuousScoringMethod: ContScoringMethod

  /** Specify the scoring (or combining) method based on the categorical target values of K neighbors. */
  def categoricalScoringMethod: CatScoringMethod

  /**
   * Contains the instance ID variable name and so refers to the name of a field in InstanceFields. Required if the
   * model has no targets, optional otherwise.
   */
  def instanceIdVariable: Option[String]

  /**
   * Defines a very small positive number to be used for "weighted" scoring methods to avoid numerical problems when
   * distance or similarity measure is zero.
   */
  def threshold: Double
}

trait HasWrappedNearestNeighborAttributes extends HasWrappedModelAttributes with HasNearestNeighborAttributes {
  override def attributes: NearestNeighborAttributes

  override def numberOfNeighbors: Int = attributes.numberOfNeighbors

  override def continuousScoringMethod: ContScoringMethod = attributes.continuousScoringMethod

  override def categoricalScoringMethod: CatScoringMethod = attributes.categoricalScoringMethod

  override def instanceIdVariable: Option[String] = attributes.instanceIdVariable

  override def threshold: Double = attributes.threshold
}

class NearestNeighborAttributes(
                                 override val functionName: MiningFunction,
                                 override val numberOfNeighbors: Int,
                                 override val continuousScoringMethod: ContScoringMethod = ContScoringMethod.average,
                                 override val categoricalScoringMethod: CatScoringMethod = CatScoringMethod.majorityVote,
                                 override val instanceIdVariable: Option[String] = None,
                                 override val threshold: Double = 0.001,
                                 override val modelName: Option[String] = None,
                                 override val algorithmName: Option[String] = None,
                                 override val isScorable: Boolean = true
                               ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable)
  with HasNearestNeighborAttributes

class NearestNeighborModelOutputs extends KNNOutputs

/*
 * Copyright (c) 2017-2024 AutoDeployAI
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

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common.Predication._
import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.{MiningSchema, Output, OutputField, Targets}
import org.pmml4s.transformations.LocalTransformations

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

/**
 * The TreeModel in PMML allows for defining either a classification or prediction structure. Each Node holds a logical
 * predicate expression that defines the rule for choosing the Node or any of the branching Nodes.
 */
class TreeModel(
                 var parent: Model,
                 override val attributes: TreeAttributes,
                 override val miningSchema: MiningSchema,
                 val node: Node,
                 override val output: Option[Output] = None,
                 override val targets: Option[Targets] = None,
                 override val localTransformations: Option[LocalTransformations] = None,
                 override val modelStats: Option[ModelStats] = None,
                 override val modelExplanation: Option[ModelExplanation] = None,
                 override val modelVerification: Option[ModelVerification] = None,
                 override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedTreeAttributes {

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.TreeModel

  // Optimize the ensemble tree model, ignore the data prepare if it's identical to the parent model
  private val ignoreDataPrepare: Boolean = if (isSubModel && localTransformations.isEmpty) {
    var ignore = true
    var i = 0
    val miningSchemaParent = parent.miningSchema
    val len = miningSchema.inputMiningFields.length
    while (i < len && ignore) {
      val mf = miningSchema.inputMiningFields(i)
      if (!mf.isDefault) {
        val mfParent = miningSchemaParent.get(mf.name)
        if (mfParent.isDefined && mf != mfParent.get) {
          ignore = false
        }
      }
      i += 1
    }
    ignore
  } else false

  /** Predicts values for a given data series using the model loaded. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = if (ignoreDataPrepare) (values, false) else prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs()

    // The root node could be a leaf
    var finalNode: Option[Node] = if (node.isLeaf) Some(node) else None
    var numMissingCount = 0
    var selected = node
    var done = false
    while (!done && selected.isSplit) {
      var child: Node = null
      var r = Predication.FALSE
      var hit = false
      var unknown = false
      var i = 0
      val children = selected.children
      val len = children.length
      while (i < len && !hit) {
        val c = children(i)
        c.eval(series) match {
          case Predication.TRUE       => {
            r = Predication.TRUE
            child = c
            hit = true
          }
          case Predication.FALSE      =>
          case Predication.SURROGATE  => {
            r = Predication.SURROGATE
            child = c
            hit = true
          }
          case Predication.UNKNOWN    => {
            unknown = true
          }
        }
        i += 1
      }

      if (!hit) {
        r = if (unknown) Predication.UNKNOWN else Predication.FALSE
      }

      if (r == Predication.SURROGATE) {
        numMissingCount += 1
      }

      if (r == Predication.UNKNOWN) {
        missingValueStrategy match {
          case MissingValueStrategy.`lastPrediction`     => {
            finalNode = Some(selected)
            done = true
          }
          case MissingValueStrategy.`nullPrediction`     =>
            done = true
          case MissingValueStrategy.`defaultChild`       => {
            child = selected.defaultChildNode.orNull
            numMissingCount += 1
          }
          case MissingValueStrategy.`weightedConfidence` => if (isClassification) {
            val total = selected.recordCount.getOrElse(Double.NaN)
            val candidates = selected.children.filter { x => x.eval(series) == UNKNOWN }
            var max = 0.0
            var i = 0
            while (i < numClasses) {
              val cls = classes(i)
              var conf = 0.0
              var j = 0
              while (j < candidates.length) {
                val candi = candidates(j)
                conf += candi.getConfidence(cls) * candi.recordCount.getOrElse(0.0) / total
                j += 1
              }

              if (conf > max) {
                max = conf
                outputs.predictedValue = cls
                outputs.confidence = conf
              }

              i += 1
            }

            done = true
          }
          case MissingValueStrategy.`aggregateNodes`     => if (isClassification) {
            val leaves = mutable.HashSet.empty[Node]
            traverseLeaves(selected, series, leaves)
            if (leaves.nonEmpty) {
              val records = new Array[Double](numClasses)
              leaves.foreach(x => {
                var i = 0
                while (i < numClasses) {
                  x.scoreDistributions.valueToDistribution.get(classes(i)).foreach(y => records(i) += y.recordCount)
                  i += 1
                }
              })

              var max = 0.0
              var i = 0
              while (i < numClasses) {
                if (records(i) > max) {
                  max = records(i)
                  outputs.predictedValue = classes(i)
                  outputs.confidence = max / records.sum
                }
                i += 1
              }
            }

            done = true
          }
          case MissingValueStrategy.`none`               =>
        }
      }

      // Handling the situation where scoring cannot continue
      if (child == null && outputs.predictedValue.isMissing) {
        noTrueChildStrategy match {
          case NoTrueChildStrategy.`returnNullPrediction` => done = true
          case NoTrueChildStrategy.`returnLastPrediction` => {
            finalNode = Some(selected)
            done = true
          }
        }
      } else if (child != null) {
        selected = child
        if (selected.isLeaf) {
          finalNode = Some(selected)
        }
      }
    }

    if (finalNode.isDefined) {
      selected = finalNode.get
      outputs.setEntityId(selected.id.orNull)
      outputs.setPredictedValue(selected.score.orNull)

      if (isClassification) {
        outputs.confidence = selected.getConfidence(outputs.predictedValue)
        if (numMissingCount > 0 && missingValuePenalty != 1.0) {
          outputs.confidence *= Math.pow(missingValuePenalty, numMissingCount)
        }
        outputs.setProbabilities(selected.probabilities)
      }
    }

    result(series, outputs)
  }

  /** The subclasses can override this method to provide classes of target inside model. */
  override def inferClasses: Array[DataVal] = {
    firstLeaf.scoreDistributions.classes
  }

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override lazy val defaultOutputFields: Array[OutputField] = {
    val result = mutable.ArrayBuilder.make[OutputField]
    result += OutputField.predictedValue(this)

    // Check if the first leaf node contains score distributions
    val leaf = firstLeaf
    if (isClassification && leaf.scoreDistributions.valueToDistribution.nonEmpty) {
      if (leaf.scoreDistributions.valueToDistribution.head._2.confidence.isDefined) {
        result += OutputField.confidence()
      }

      if (leaf.scoreDistributions.valueToDistribution.head._2.probability.isDefined) {
        result += OutputField.probability()
        for (cls <- classes) {
          result += OutputField.probability(cls)
        }
      }
    }

    if (leaf.id.isDefined) {
      result += OutputField.nodeId()
    }

    result.result()
  }

  private def firstLeaf: Node = {
    var n = node;
    while (n.isSplit) n = n.children(0);
    n
  }

  // The method is not used anymore, it's moved into predict directly for performance
  private def traverseNode(n: Node, series: Series): (Option[Node], Predication) = {
    var unknown = false
    
    // The performance of while loop is better than for comprehension
    var i = 0
    while (i < n.children.length) {
      val child = n.children(i)
      child.eval(series) match {
        case TRUE      => return (Some(child), TRUE)
        case SURROGATE => return (Some(child), SURROGATE)
        case UNKNOWN   => unknown = true
        case _         =>
      }
      i += 1
    }

    (None, if (unknown) UNKNOWN else FALSE)
  }

  private def traverseLeaves(n: Node, series: Series, leaves: mutable.Set[Node]): Unit = {
    if (n.isLeaf) {
      leaves += n
    } else {
      val candidates = ArrayBuffer.empty[Node]
      var i = 0
      var done = false
      while (!done && i < n.size) {
        n(i).eval(series) match {
          case TRUE      => candidates += n(i); done = true
          case SURROGATE => candidates += n(i); done = true
          case UNKNOWN   => candidates += n(i)
          case _         =>
        }

        i += 1
      }

      i = 0
      while (i < candidates.length) {
        val child = candidates(i)
        traverseLeaves(child, series, leaves)
        i += 1
      }
    }
  }

  override def createOutputs(): TreeOutputs = new TreeOutputs
}

/**
 * Defines a strategy for dealing with missing values.
 */
object MissingValueStrategy extends Enumeration {
  type MissingValueStrategy = Value

  /**
   * - lastPrediction: If a Node's predicate evaluates to UNKNOWN while traversing the tree, evaluation is stopped
   * and the current winner is returned as the final prediction.
   * - nullPrediction: If a Node's predicate value evaluates to UNKNOWN while traversing the tree, abort the scoring
   * process and give no prediction.
   * - defaultChild: If a Node's predicate value evaluates to UNKNOWN while traversing the tree, evaluate the
   * attribute defaultChild which gives the child to continue traversing with. Requires the presence of the attribute
   * defaultChild in every non-leaf Node.
   * - weightedConfidence: If a Node's predicate value evaluates to UNKNOWN while traversing the tree, the
   * confidences for each class is calculated from scoring it and each of its sibling Nodes in turn (excluding any
   * siblings whose predicates evaluate to FALSE). The confidences returned for each class from each sibling Node
   * that was scored are weighted by the proportion of the number of records in that Node, then summed to produce a
   * total confidence for each class. The winner is the class with the highest confidence. Note that
   * weightedConfidence should be applied recursively to deal with situations where several predicates within the
   * tree evaluate to UNKNOWN during the scoring of a case.
   * - aggregateNodes: If a Node's predicate value evaluates to UNKNOWN while traversing the tree, we consider
   * evaluation of the Node's predicate being TRUE and follow this Node. In addition, subsequent Nodes to the initial
   * Node are evaluated as well. This procedure is applied recursively for each Node being evaluated until a leaf
   * Node is reached. All leaf Nodes being reached by this procedure are aggregated such that for each value
   * attribute of such a leaf Node's ScoreDistribution element the corresponding recordCount attribute values are
   * accumulated. The value associated with the highest recordCount accumulated through this procedure is predicted.
   * The basic idea of missingValueStrategy aggregateNodes is to aggregate all leaf Nodes which may be reached by a
   * record with one or more missing values considering all possible values. Strategy aggregateNodes calculates a
   * virtual Node and predicts a score according to this virtual Node. Requires the presence of attribute recordCount
   * in all ScoreDistribution elements.
   * - none: Comparisons with missing values other than checks for missing values always evaluate to FALSE. If no
   * rule fires, then use the noTrueChildStrategy to decide on a result. This option requires that missing values be
   * handled after all rules at the Node have been evaluated.
   * Note: In contrast to lastPrediction, evaluation is carried on instead of stopping immediately upon first
   * discovery of a Node who's predicate value cannot be determined due to missing values.
   */
  val lastPrediction, nullPrediction, defaultChild, weightedConfidence, aggregateNodes, none = Value
}

/**
 * Defines what to do in situations where scoring cannot reach a leaf node.
 */
object NoTrueChildStrategy extends Enumeration {
  type NoTrueChildStrategy = Value
  val returnNullPrediction, returnLastPrediction = Value
}

/**
 * Indicates whether non-leaf Nodes in the tree model have exactly two children, or an unrestricted number of children.
 */
object SplitCharacteristic extends Enumeration {
  type SplitCharacteristic = Value
  val binarySplit, multiSplit = Value
}

import org.pmml4s.model.MissingValueStrategy._
import org.pmml4s.model.NoTrueChildStrategy._
import org.pmml4s.model.SplitCharacteristic._

trait HasTreeAttributes extends HasModelAttributes {
  /**
   * Defines a strategy for dealing with missing values.
   */
  def missingValueStrategy: MissingValueStrategy

  /**
   * Defines a penalty applied to confidence calculation when missing value handling is performed.
   */
  def missingValuePenalty: Double

  /**
   * Defines what to do in situations where scoring cannot reach a leaf node.
   */
  def noTrueChildStrategy: NoTrueChildStrategy

  /**
   * Indicates whether non-leaf Nodes in the tree model have exactly two children, or an unrestricted number of
   * children.
   * In the case of multiSplit, it means that each Node may have 0 or more child Nodes. In the case of binarySplit,
   * it means that each Node must have either 0 or 2 child Nodes.
   */
  def splitCharacteristic: SplitCharacteristic
}

/**
 * Holds attributes of a Tree model
 */
class TreeAttributes(
                      override val functionName: MiningFunction,
                      override val modelName: Option[String] = None,
                      override val algorithmName: Option[String] = None,
                      override val isScorable: Boolean = true,
                      val missingValueStrategy: MissingValueStrategy = MissingValueStrategy.none,
                      val missingValuePenalty: Double = 1.0,
                      val noTrueChildStrategy: NoTrueChildStrategy = NoTrueChildStrategy.returnNullPrediction,
                      val splitCharacteristic: SplitCharacteristic = SplitCharacteristic.multiSplit)
  extends ModelAttributes(functionName, modelName, algorithmName, isScorable) with HasTreeAttributes

trait HasWrappedTreeAttributes extends HasWrappedModelAttributes with HasTreeAttributes {

  override def attributes: TreeAttributes

  def missingValueStrategy: MissingValueStrategy = attributes.missingValueStrategy

  def missingValuePenalty: Double = attributes.missingValuePenalty

  def noTrueChildStrategy: NoTrueChildStrategy = attributes.noTrueChildStrategy

  def splitCharacteristic: SplitCharacteristic = attributes.splitCharacteristic
}

/**
 * This element is an encapsulation for either defining a split or a leaf in a tree model.
 * Every Node contains a predicate that identifies a rule for choosing itself or any of its siblings.
 * A predicate may be an expression composed of other nested predicates.
 */
class Node(
            val predicate: Predicate,
            val children: Array[Node],
            val id: Option[String] = None,
            val score: Option[DataVal] = None,
            val recordCount: Option[Double] = None,
            val defaultChild: Option[String] = None,
            val scoreDistributions: ScoreDistributions = new ScoreDistributions,
            val partition: Option[Partition] = None,
            val embeddedModel: Option[EmbeddedModel] = None) extends Predicate with HasScoreDistributions {

  def eval(series: Series): Predication = predicate.eval(series)

  val isLeaf: Boolean = children.isEmpty

  val isSplit: Boolean = children.nonEmpty

  val size: Int = children.length

  def apply(i: Int): Node = children(i)

  val defaultChildNode: Option[Node] = defaultChild.flatMap {
    x =>
      children.find {
        y => y.id match {
          case Some(id) => id == x
          case None => false
        }
      }
  }
}

class TreeOutputs extends MixedClsWithRegOutputs with MutableConfidence with MutableEntityId {
  override def modelElement: ModelElement = ModelElement.TreeModel

  override def clear(): this.type = {
    predictedValue = DataVal.NULL
    predictedDisplayValue = null
    probabilities = Map.empty
    confidence = Double.NaN
    entityId = DataVal.NULL
    this
  }
}

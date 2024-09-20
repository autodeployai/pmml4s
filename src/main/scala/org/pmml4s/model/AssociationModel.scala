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

import org.pmml4s.Since
import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.Algorithm.Algorithm
import org.pmml4s.metadata.RankBasis.RankBasis
import org.pmml4s.metadata.RankOrder.RankOrder
import org.pmml4s.metadata._
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.Utils

import scala.collection.{immutable, mutable}

/**
 * The Association Rule model represents rules where some set of items is associated to another set of items. For
 * example a rule can express that a certain product or set of products is often bought in combination with a certain
 * set of other products, also known as Market Basket Analysis. An Association Rule model typically has two variables:
 * one for grouping records together into transactions (usageType="group") and another that uniquely identifies each
 * record (usageType="active"). Alternatively, association rule models can be built on regular data, where each category
 * of each categorical field is an item. Yet another possible format of data is a table with true/false values, where
 * only the fields having true value in a record are considered valid items.
 *
 * An Association Rule model consists of four major parts:
 * - Model attributes
 * - Items
 * - ItemSets
 * - AssociationRules
 */
class AssociationModel(
                        var parent: Model,
                        override val attributes: AssociationAttributes,
                        override val miningSchema: MiningSchema,
                        val items: Array[Item],
                        val itemsets: Array[Itemset],
                        val associationRules: Array[AssociationRule],
                        override val output: Option[Output] = None,
                        override val targets: Option[Targets] = None,
                        override val localTransformations: Option[LocalTransformations] = None,
                        override val modelStats: Option[ModelStats] = None,
                        override val modelExplanation: Option[ModelExplanation] = None,
                        override val modelVerification: Option[ModelVerification] = None,
                        override val extensions: immutable.Seq[Extension] = immutable.Seq.empty
                      ) extends Model with HasWrappedAssociationAttributes {
  val groupField: Field = {
    val fs = miningSchema.getByUsageType(UsageType.group)
    if (fs.nonEmpty) field(fs.head.name) else null
  }

  val activeField: Field = if (groupField != null)
    field(miningSchema.getByUsageType(UsageType.active).head.name)
  else null

  require(groupField == null || activeField != null, "An active field is required when a group field is present")

  // An active field could be a derived field
  private val activeNames = items.map(_.getName).filter(_.isDefined).map(_.get).distinct
  private val activeFields: Array[Field] = if (activeNames.nonEmpty) activeNames.map(x => field(x)) else inputFields

  private val idToItem: Map[String, String] = items.map(x => (x.id, x.toString)).toMap
  private val idToItemset: Map[String, Set[String]] =
    itemsets.map(x => (x.id, x.itemRefs.map(y => idToItem(y.itemRef)))).toMap

  {
    var i = 0
    while (i < associationRules.length) {
      associationRules(i).init(idToItemset, i)
      i += 1
    }
  }

  @transient private var lastGroup: Any = null
  @transient private val lastItemset: mutable.Set[String] = mutable.Set.empty

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.AssociationModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val itemset: scala.collection.Set[String] = if (groupField != null) {
      val group = groupField.get(series)
      val item = activeField.get(series)
      if (group != lastGroup) {
        lastItemset.clear()
        lastGroup = group
      }

      if (item != null) {
        lastItemset += item.toString
      }
      lastItemset
    } else {
      lastItemset.clear()
      for (elem <- activeFields) {
        elem.dataType match {
          case BooleanType => if (Utils.toBoolean(elem.get(series))) lastItemset += elem.name
          case _           => {
            val value = elem.get(series)
            if (value != null) {
              lastItemset += (elem.name + "=" + value)
            }
          }
        }
      }
      lastItemset
    }

    val outputs = createOutputs()

    val criteria = topCriteria
    import org.pmml4s.metadata.RankBasis._
    outputs.rules = criteria.map(x => {
      val selects = associationRules.filter(_.fire(itemset, x._1._1))
      val sorted = if (selects.nonEmpty) x._1._2 match {
        case `confidence` => selects.sortBy(_.confidence)
        case `support`    => selects.sortBy(_.support)
        case `lift`       => if (selects.head.lift.isDefined) selects.sortBy(_.lift.get) else selects.sortBy(_.confidence)
        case `leverage`   => if (selects.head.leverage.isDefined) selects.sortBy(_.leverage.get) else selects.sortBy(_.confidence)
        case `affinity`   => if (selects.head.affinity.isDefined) selects.sortBy(_.affinity.get) else selects.sortBy(_.confidence)
      } else selects

      import org.pmml4s.metadata.RankOrder._
      (x._1, x._1._3 match {
        case `descending` => sorted.take(x._2)
        case `ascending`  => sorted.takeRight(x._2).reverse
      })
    })

    result(series, outputs)
  }

  def topCriteria: Map[(Algorithm, RankBasis, RankOrder), Int] = {
    val ofs = candidateOutputFields
    ofs.groupBy(_.criterion).map(x => (x._1, x._2.maxBy(_.rank).rank))
  }

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): AssociationOutputs = new AssociationOutputs
}

/**
 * Obviously the id of an Item must be unique. Furthermore the Item values must be unique, or if they are not unique
 * then attributes field and category must distiguish them. That is, an AssocationModel must not have different
 * instances of Item where the values of the value, field, and category attribute are all the same. The entries in
 * mappedValue may be the same, though. Here are some examples of Items:
 *
 * @param id          An identification to uniquely identify an item.
 * @param value       The value of the item as in the input data.
 * @param field
 * @param category
 * @param mappedValue Optional, a value to which the original item value is mapped. For instance, this could be a
 *                    product name if the original value is an EAN code.
 * @param weight      The weight of the item. For example, the price or value of an item.
 */
class Item(val id: String,
           val value: Any,
           @Since("4.3")
           val field: Option[Field] = None,
           @Since("4.3")
           val category: Option[String] = None,
           val mappedValue: Option[String] = None,
           val weight: Option[Double] = None) extends PmmlElement {

  override def toString: String = {
    val str = value.toString
    if (field.isEmpty && category.isEmpty) {
      str
    } else if (field.isDefined && category.isDefined) {
      if (str.contains('=')) {
        str
      } else if (field.get.name.contains('=')) {
        field.get.name
      } else if (category.get != "true") {
        field.get.name + "=" + category.get
      } else {
        field.get.name + "=" + str
      }
    } else if (field.isDefined) {
      if (field.get.name.contains('=')) {
        field.get.name
      } else {
        field.get.name + "=" + str
      }
    } else {
      str
    }
  }

  def getName: Option[String] = field.map(_.name) orElse ({
    if (value.toString.contains('=')) {
      Some(value.toString.split('=')(0))
    } else None
  })
}

/**
 * Item references point to elements of type Item
 *
 * @param itemRef Contains the identification of an item.
 */
class ItemRef(val itemRef: String) extends PmmlElement

/**
 *
 * @param itemRefs      Item references point to elements of type Item
 * @param id            An identification to uniquely identify an Itemset.
 * @param support       The relative support of the Itemset:
 *                      support(set) = (number of transactions containing the set) / (total number of transactions)
 * @param numberOfItems The number of Items contained in this Itemset
 */
class Itemset(val itemRefs: Set[ItemRef],
              val id: String,
              val support: Option[Double] = None,
              val numberOfItems: Option[Int] = None) extends PmmlElement {

}

/**
 * We consider association rules of the form "<antecedent itemset> => <consequent itemset>" next:
 *
 * @param antecedent The id value of the itemset which is the antecedent of the rule. We represent the itemset by the
 *                   letter A.
 * @param consequent The id value of the itemset which is the consequent of the rule. We represent the itemset by the
 *                   letter C.
 * @param support    The support of the rule, that is, the relative frequency of transactions that contain A and C:
 *                   support(A->C) = support(A+C)
 * @param confidence The confidence of the rule:
 *                   confidence(A->C) = support(A+C) / support(A)
 * @param lift       A very popular measure of interestingness of a rule is lift. Lift values greater than 1.0 indicate
 *                   that transactions containing A tend to contain C more often than transactions that do not contain A:
 *                   lift(A->C) = confidence(A->C) / support(C)
 * @param leverage   Another measure of interestingness is leverage. An association with higher frequency and lower lift
 *                   may be more interesting than an alternative rule with lower frequency and higher lift. The former
 *                   can be more important in practice because it applies to more cases. The value is the difference
 *                   between the observed frequency of A+C and the frequency that would be expected if A and C were
 *                   independent:
 *                   leverage(A->C) = support(A->C) - support(A)*support(C)
 * @param affinity   Also known as Jaccard Similarity, affinity is a measure of the transactions that contain both the
 *                   antecedent and consequent (intersect) compared to those that contain the antecedent or the
 *                   consequent (union):
 *                   affinity(A->C) = support(A+C) / [ support(A) + support(C) - support(A+C)]
 * @param id         An identification to uniquely identify an association rule.
 */
class AssociationRule(val antecedent: String,
                      val consequent: String,
                      val support: Double,
                      val confidence: Double,
                      val lift: Option[Double] = None,
                      val leverage: Option[Double] = None,
                      val affinity: Option[Double] = None,
                      val id: Option[String] = None) extends HasPredictedValue
  with HasEntityId
  with HasConfidence
  with PmmlElement {

  var antecedentItemset: Set[String] = _
  var consequentItemset: Set[String] = _

  // 1 -based index
  var indexId: String = "-1"

  lazy val antecedentRule: String =
    (if (antecedentItemset.size > 1) antecedentItemset.mkString("{", " , ", "}") else antecedentItemset.head)

  lazy val consequentRule: String =
    (if (consequentItemset.size > 1) consequentItemset.mkString("{", " , ", "}") else consequentItemset.head)

  lazy val rule: String = antecedentRule + " -> " + consequentRule

  def init(idToItemset: Map[String, Set[String]], index: Int): Unit = {
    antecedentItemset = idToItemset(antecedent)
    consequentItemset = idToItemset(consequent)
    indexId = (index + 1).toString
  }

  import org.pmml4s.metadata.Algorithm._

  def fire(itemset: scala.collection.Set[String], algorithm: Algorithm): Boolean = algorithm match {
    case `recommendation`          => antecedentItemset.subsetOf(itemset)
    case `exclusiveRecommendation` => antecedentItemset.subsetOf(itemset) && !consequentItemset.subsetOf(itemset)
    case `ruleAssociation`         => antecedentItemset.subsetOf(itemset) && consequentItemset.subsetOf(itemset)
  }

  lazy override val predictedValue: DataVal = DataVal.from(consequentRule)

  lazy override val entityId: DataVal = DataVal.from(id.getOrElse(indexId))
}

trait HasAssociationAttributes extends HasModelAttributes {

  /**
   * The number of transactions contained in the input data.
   */
  def numberOfTransactions: Int

  /**
   * The number of items contained in the largest transaction.
   */
  def maxNumberOfItemsPerTA: Option[Int]

  /**
   * The average number of items contained in a transaction.
   */
  def avgNumberOfItemsPerTA: Option[Double]

  /**
   * The minimum relative support value (#supporting transactions / #total transactions) satisfied by all rules.
   */
  def minimumSupport: Double

  /**
   * The minimum confidence value satisfied by all rules. Confidence is calculated as (support (rule) /
   * support(antecedent)).
   */
  def minimumConfidence: Double

  /**
   * The maximum number of items contained in a rule which was used to limit the number of rules.
   */
  def lengthLimit: Option[Int]

  /**
   * The number of different items contained in the input data. This number may be greater than or equal to the number
   * of items contained in the model. The value will be greater if any items in the input data are excluded from the
   * model, as a consequence of not being referenced by the model.
   */
  def numberOfItems: Int

  /**
   * The number of itemsets contained in the model.
   */
  def numberOfItemsets: Int

  /**
   * The number of rules contained in the model.
   */
  def numberOfRules: Int

}

trait HasWrappedAssociationAttributes extends HasWrappedModelAttributes with HasAssociationAttributes {
  override def attributes: AssociationAttributes

  override def numberOfTransactions: Int = attributes.numberOfTransactions

  override def maxNumberOfItemsPerTA: Option[Int] = attributes.maxNumberOfItemsPerTA

  override def avgNumberOfItemsPerTA: Option[Double] = attributes.avgNumberOfItemsPerTA

  override def minimumSupport: Double = attributes.minimumSupport

  override def minimumConfidence: Double = attributes.minimumConfidence

  override def lengthLimit: Option[Int] = attributes.lengthLimit

  override def numberOfItems: Int = attributes.numberOfItems

  override def numberOfItemsets: Int = attributes.numberOfItemsets

  override def numberOfRules: Int = attributes.numberOfRules
}

class AssociationAttributes(
                             val numberOfTransactions: Int,
                             val minimumSupport: Double,
                             val minimumConfidence: Double,
                             val numberOfItems: Int,
                             val numberOfItemsets: Int,
                             val numberOfRules: Int,
                             val maxNumberOfItemsPerTA: Option[Int] = None,
                             val avgNumberOfItemsPerTA: Option[Double] = None,
                             val lengthLimit: Option[Int] = None,
                             override val functionName: MiningFunction = MiningFunction.associationRules,
                             override val modelName: Option[String] = None,
                             override val algorithmName: Option[String] = None,
                             override val isScorable: Boolean = true
                           ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable)
  with HasAssociationAttributes {
  def this(attributes: ModelAttributes,
           numberOfTransactions: Int,
           minimumSupport: Double,
           minimumConfidence: Double,
           numberOfItems: Int,
           numberOfItemsets: Int,
           numberOfRules: Int,
           maxNumberOfItemsPerTA: Option[Int],
           avgNumberOfItemsPerTA: Option[Double],
           lengthLimit: Option[Int]) = {
    this(numberOfTransactions, minimumSupport, minimumConfidence, numberOfItems, numberOfItemsets, numberOfRules,
      maxNumberOfItemsPerTA, avgNumberOfItemsPerTA, lengthLimit, attributes.functionName, attributes.modelName,
      attributes.algorithmName, attributes.isScorable)
  }
}

class AssociationOutputs extends ModelOutputs with HasAssociationRules {
  var rules: Map[(Algorithm, RankBasis, RankOrder), Array[AssociationRule]] = Map.empty

  def setRules(rules: Map[(Algorithm, RankBasis, RankOrder), Array[AssociationRule]]): this.type = {
    this.rules = rules
    this
  }

  override def getRules(criterion: (Algorithm, RankBasis, RankOrder)): Array[AssociationRule] = rules(criterion)

  override def modelElement: ModelElement = ModelElement.AssociationModel

  override def clear(): this.type = {
    rules = Map.empty
    this
  }
}


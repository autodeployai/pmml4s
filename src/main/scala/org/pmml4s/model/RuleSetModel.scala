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

import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.{MiningSchema, Output, OutputField, Targets}
import org.pmml4s.model.Criterion.Criterion
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.xml.ElemTags.{COMPOUND_RULE, SIMPLE_RULE}

import scala.collection.{immutable, mutable}

/**
 * Ruleset models can be thought of as flattened decision tree models. A ruleset consists of a number of rules. Each
 * rule contains a predicate and a predicted class value, plus some information collected at training or testing time on
 * the performance of the rule.
 */
class RuleSetModel(var parent: Model,
                   override val attributes: ModelAttributes,
                   override val miningSchema: MiningSchema,
                   val ruleSet: RuleSet,
                   override val output: Option[Output] = None,
                   override val targets: Option[Targets] = None,
                   override val localTransformations: Option[LocalTransformations] = None,
                   override val modelStats: Option[ModelStats] = None,
                   override val modelExplanation: Option[ModelExplanation] = None,
                   override val modelVerification: Option[ModelVerification] = None,
                   override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedModelAttributes {

  // If more than one method is included, the first method is used as the default method for scoring
  var criterion: Criterion = ruleSet.ruleSelectionMethods.head.criterion

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.RuleSetModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs().
      setPredictedValue(ruleSet.defaultScore.orNull).
      setConfidence(ruleSet.defaultConfidence.getOrElse(Double.NaN))

    import Criterion._
    criterion match {
      case `firstHit` => {
        val first = ruleSet.first(series)
        if (first.isDefined) {
          outputs.predictedValue = first.get.score
          outputs.confidence = first.get.confidence
        }
      }
      case _          => {
        val rules = ruleSet.fire(series)
        if (rules.nonEmpty) {
          if (criterion == `weightedMax`) {
            val max = rules.maxBy(_.weight)
            outputs.predictedValue = max.score
            outputs.confidence = max.confidence
          } else {
            val one = rules.groupBy(_.score).map(x => (x._1, x._2.map(_.weight).sum)).maxBy(_._2)
            outputs.predictedValue = one._1
            outputs.confidence = one._2 / rules.length
          }
        }
      }
    }

    result(series, outputs)
  }

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override def defaultOutputFields: Array[OutputField] = {
    if (isClassification) {
      val result = mutable.ArrayBuilder.make[OutputField]
      result.sizeHint(2)
      result += OutputField.predictedValue(this)
      result += OutputField.confidence()

      result.result()
    } else {
      Array.empty
    }
  }

  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): RuleSetOutputs = new RuleSetOutputs

  /** Returns all candidates of criteria, */
  def criteria: Array[Criterion] = ruleSet.ruleSelectionMethods.map(_.criterion)
}

/**
 *
 * @param ruleSelectionMethods specifies how to select rules from the ruleset to score a new case. If more than one
 *                             method is included, the first method is used as the default method for scoring, but the
 *                             other methods included may be selected by the application wishing to perform scoring as
 *                             valid alternative methods.
 * @param scoreDistributions   describe the distribution of the predicted value in the test/training data.
 * @param rules                contains 0 or more rules which comprise the ruleset.
 * @param recordCount          The number of training/test cases to which the ruleset was applied to generate support
 *                             and
 *                             confidence measures for individual rules.
 * @param nbCorrect            indicates the number of training/test instances for which the default score is correct.
 * @param defaultScore         The value of score in a RuleSet serves as the default predicted value when scoring a
 *                             case no
 *                             rules in the ruleset fire.
 * @param defaultConfidence    provides a confidence to be returned with the default score (when scoring a case and no
 *                             rules in the ruleset fire).
 */
class RuleSet(val ruleSelectionMethods: Array[RuleSelectionMethod],
              val scoreDistributions: ScoreDistributions,
              val rules: Array[Rule],
              val recordCount: Option[Int],
              val nbCorrect: Option[Int],
              val defaultScore: Option[DataVal],
              val defaultConfidence: Option[Double]) extends PmmlElement {

  def first(series: Series): Option[SimpleRule] = {
    val len = rules.length
    var i = 0
    while (i < len) {
      val rule = rules(i)
      val res = rule.first(series)
      if (res.isDefined)
        return res
      i += 1
    }
    None
  }

  def fire(series: Series): Array[SimpleRule] = {
    rules.flatMap(_.fire(series))
  }
}


/**
 * Describes how rules are selected to apply the model to a new case
 *
 * @param criterion explains how to determine and rank predictions and their associated confidences from the ruleset in
 *                  case multiple rules fire.
 */
class RuleSelectionMethod(val criterion: Criterion) extends PmmlElement

object Criterion extends Enumeration {
  type Criterion = Value

  /**
   * There are many many possible ways of applying rulesets, but three useful approaches are covered.
   *
   * - firstHit: First firing rule is chosen as the predicted class, and the confidence is the confidence of that rule.
   * If further predictions and confidences are required, a search for the next firing rule that chooses a different
   * predicted class is made, and so on.
   *
   * - weightedSum: Calculate the total weight for each class by summing the weights for each firing rule which predicts
   * that class. The prediction with the highest total weight is then selected. The confidence is the total confidence
   * of the winning class divided by the number of firing rules. If further predictions and confidences are required,
   * the process is repeated to find the class with the second highest total weight, and so on. Note that if two or more
   * classes are assigned the same weight, the winning class is the one that appears first in the data dictionary
   * values.
   *
   * - weightedMax: Select the firing rule with the highest weight. The confidence returned is the confidence of the
   * selected rule. Note that if two firing rules have the same weight, the rule that occurs first in the ruleset is
   * chosen.
   */
  val weightedSum, weightedMax, firstHit = Value
}

sealed trait Rule {
  def predicate: Predicate

  def fire(series: Series): Array[SimpleRule]

  def first(series: Series): Option[SimpleRule]
}

object Rule {
  val emptySimpleRuleArray: Array[SimpleRule] = Array.empty

  val values: Set[String] = Set(SIMPLE_RULE, COMPOUND_RULE)

  def contains(s: String): Boolean = values.contains(s)
}

/**
 * SimpleRule consists of an identifier, a predicate, a score and information on rule performance.
 *
 * @param predicate          the condition upon which the rule fires. For more details on PREDICATE see the section on
 *                           predicates in TreeModel. This explains how predicates are described and evaluated and how
 *                           missing values are handled.
 * @param scoreDistributions Describes the distribution of the predicted value for instances where the rule fires in the
 *                           training/test data.
 * @param score              The predicted value when the rule fires.
 * @param id                 The value of id serves as a unique identifier for the rule. Must be unique within the
 *                           ruleset.
 * @param recordCount        The number of training/test instances on which the rule fired.
 * @param nbCorrect          Indicates the number of training/test instances on which the rule fired and the prediction
 *                           was correct.
 * @param confidence         Indicates the confidence of the rule.
 * @param weight             Indicates the relative importance of the rule. May or may not be equal to the confidence.
 */
class SimpleRule(val predicate: Predicate,
                 val scoreDistributions: ScoreDistributions,
                 val score: DataVal,
                 val id: Option[String] = None,
                 val recordCount: Option[Int] = None,
                 val nbCorrect: Option[Int] = None,
                 val confidence: Double = 1.0,
                 val weight: Double = 1.0
                ) extends Rule with HasScoreDistributions with PmmlElement {
  override def fire(series: Series): Array[SimpleRule] = if (predicate.eval(series) == Predication.TRUE)
    Array(this) else
    Rule.emptySimpleRuleArray

  override def first(series: Series): Option[SimpleRule] = if (predicate.eval(series) == Predication.TRUE)
    Some(this) else None
}

/**
 * CompoundRule consists of a predicate and one or more rules. CompoundRules offer a shorthand for a more compact
 * representation of rulesets and suggest a more efficient execution mechanism.
 *
 * @param predicate the condition upon which the rule fires.
 * @param rules     One or more rules that are contained within the CompoundRule. Each of these rules may be a
 *                  SimpleRule or a CompoundRule.
 */
class CompoundRule(val predicate: Predicate, val rules: Array[Rule]) extends Rule with PmmlElement {
  override def fire(series: Series): Array[SimpleRule] = if (predicate.eval(series) == Predication.TRUE) {
    rules.flatMap(_.fire(series))
  } else Rule.emptySimpleRuleArray

  override def first(series: Series): Option[SimpleRule] = if (predicate.eval(series) == Predication.TRUE) {
    val len = rules.length
    var i = 0
    while (i < len) {
      val rule = rules(i)
      val res = rule.first(series)
      if (res.isDefined)
        return res
      i += 1
    }
    None
  } else None
}

class RuleSetOutputs extends ModelOutputs with MutablePredictedValue with MutableConfidence {
  override def modelElement: ModelElement = ModelElement.RuleSetModel

  override def clear(): this.type = {
    super[MutablePredictedValue].clear()
    super[MutableConfidence].clear()
    this
  }
}


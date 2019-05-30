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
package org.pmml4s.metadata

import org.pmml4s.PmmlDeprecated

/** Result Features */
object ResultFeature extends Enumeration {
  type ResultFeature = Value
  val predictedValue, predictedDisplayValue, transformedValue, decision, probability, affinity, residual, standardError,
  clusterId, clusterAffinity, entityId, entityAffinity, warning, ruleValue, reasonCode, antecedent, consequent, rule,
  ruleId, confidence, support, lift, leverage = Value
}

/**
 * Specifies which feature of an association rule to return.
 * This attribute has been deprecated as of PMML 4.2.
 * The rule feature values can now be specified in the feature attribute.
 */
@PmmlDeprecated(since = "4.2")
object RuleFeature extends Enumeration {
  type RuleFeature = Value
  val antecedent, consequent, rule, ruleId, confidence, support, lift, leverage, affinity = Value
}

/**
 * Specifies which scoring algorithm to use when computing the output value.
 * It applies only to Association Rules models.
 */
object Algorithm extends Enumeration {
  type Algorithm = Value
  val recommendation, exclusiveRecommendation, ruleAssociation = Value
}

/**
 * Applies only to Association Rules and is used to specify which criterion is used to sort the output result.
 * For instance, the result could be sorted by the confidence, support or lift of the rules.
 */
object RankBasis extends Enumeration {
  type RankBasis = Value
  val confidence, support, lift, leverage, affinity = Value
}

/**
 * Determines the sorting order when ranking the results. The default behavior (rankOrder="descending")
 * indicates that the result with the highest rank will appear first on the sorted list.
 */
object RankOrder extends Enumeration {
  type RankOrder = Value
  val descending, ascending = Value
}

import org.pmml4s.common._
import org.pmml4s.metadata.Algorithm.Algorithm
import org.pmml4s.metadata.RankBasis.RankBasis
import org.pmml4s.metadata.RankOrder.RankOrder
import org.pmml4s.metadata.ResultFeature.ResultFeature
import org.pmml4s.metadata.RuleFeature.RuleFeature
import org.pmml4s.model.Model
import org.pmml4s.transformations.Expression

/**
 * OutputField elements specify names, types and rules for calculating specific result features. This information can
 * be used while writing an output table.
 */
class OutputField(override val name: String,
                  override val displayName: Option[String],
                  override val dataType: DataType,
                  override val opType: OpType,
                  val feature: ResultFeature = ResultFeature.predictedValue,
                  val targetField: Option[String] = None,
                  val value: Option[Any] = None,
                  val ruleFeature: RuleFeature = RuleFeature.consequent,
                  val algorithm: Algorithm = Algorithm.exclusiveRecommendation,
                  val rank: Int = 1,
                  val rankBasis: RankBasis = RankBasis.confidence,
                  val rankOrder: RankOrder = RankOrder.descending,
                  val isMultiValued: Boolean = false,
                  val segmentId: Option[String] = None,
                  val isFinalResult: Boolean = true,
                  val decisions: Option[Decisions] = None,
                  val expr: Option[Expression] = None) extends AbstractField with PmmlElement {

  require(feature != ResultFeature.transformedValue || (expr.isDefined || segmentId.isDefined),
    "For the `transformedValue` result feature, OutputField must contain an EXPRESSION, unless it is used to refer to a " +
      "transformed value of a segment model through the segmentID attribute.")

  override def fieldType: FieldType = FieldType.OutputField

  def criterion: (Algorithm, RankBasis, RankOrder) = (algorithm, rankBasis, rankOrder)

  override def toString = s"OutputField(name=$name, displayName=$displayName, dataType=$dataType, opType=$opType, feature=$feature, targetField=$targetField, value=$value, ruleFeature=$ruleFeature, algorithm=$algorithm, rank=$rank, rankBasis=$rankBasis, rankOrder=$rankOrder, isMultiValued=$isMultiValued, segmentId=$segmentId, isFinalResult=$isFinalResult, decisions=$decisions, expr=$expr)"
}

object OutputField {

  def predictedValue(model: Model): OutputField = {
    if (model.hasTarget) predictedValue(model.targetField) else {
      new OutputField("PredictedValue",
        Some(s"Predicted value"),
        if (model.isRegression) RealType else StringType,
        if (model.isRegression) OpType.continuous else OpType.nominal)
    }
  }

  def predictedValue(target: Field): OutputField = {
    new OutputField("PredictedValue",
      Some(s"Predicted value of ${target.name}"),
      if (target.dataType == IntegerType) RealType else target.dataType,
      target.opType)
  }

  def predictedValue(name: String, displayName: String, dataType: DataType, opType: OpType): OutputField = {
    new OutputField(name,
      Option(displayName),
      dataType,
      opType)
  }

  def predictedDisplayValue(name: String, displayName: String): OutputField = {
    new OutputField(name,
      Option(displayName),
      StringType,
      OpType.nominal,
      ResultFeature.predictedDisplayValue)
  }

  def probability(value: Any = null) = {
    if (value == null) {
      new OutputField("Probability",
        Some("Probability of predicted value"),
        RealType,
        OpType.continuous,
        ResultFeature.probability)
    } else {
      new OutputField(s"Probability_$value",
        Some(s"Probability of $value"),
        RealType,
        OpType.continuous,
        ResultFeature.probability,
        value = Option(value))
    }
  }

  def confidence(): OutputField = {
    new OutputField("Confidence",
      Some("Confidence of predicted value"),
      RealType,
      OpType.continuous,
      ResultFeature.confidence)
  }

  def nodeId(): OutputField = {
    entityId("Node_ID", "ID of hit node")
  }

  def entityId(name: String, displayName: String): OutputField = {
    new OutputField(name,
      Option(displayName),
      StringType,
      OpType.nominal,
      ResultFeature.entityId)
  }

  def affinity(name: String, displayName: String): OutputField = {
    new OutputField(name,
      Option(displayName),
      RealType,
      OpType.continuous,
      ResultFeature.affinity)
  }

  def reasonCode(rank: Int): OutputField = {
    new OutputField(s"ReasonCode_${rank}",
      Option(s"Reason code ${rank}"),
      StringType,
      OpType.nominal,
      ResultFeature.reasonCode)
  }
}

/**
 * The Decisions element contains an element Decision for every possible value of the decision.
 */
class Decisions(
                 val decisions: Array[Decision],
                 val businessProblem: Option[String],
                 val description: Option[String]) extends PmmlElement

class Decision(
                val value: String,
                val displayValue: Option[String],
                val description: Option[String]) extends PmmlElement

/**
 * The Output section in the model specifies names for columns in an output table and describes how to compute the
 * corresponding values.
 */
trait HasOutputFields {
  def outputFields: Array[OutputField]

  def finalOutputFields: Array[OutputField] = outputFields.filter(_.isFinalResult)

  def containInterResults: Boolean = outputFields.exists(x => !x.isFinalResult)
}

/**
 * Output element describes a set of result values that can be returned from a model.
 */
class Output(override val outputFields: Array[OutputField])
  extends HasOutputFields with HasField with PmmlElement {
  lazy val map: Map[String, OutputField] = outputFields.map(x => (x.name, x)).toMap

  /**
   * Returns the field of a given name, None if a field with the given name does not exist
   */
  override def getField(name: String): Option[Field] = map.get(name)
}

trait HasOutput {
  self: Model =>
  def output: Option[Output]

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  def defaultOutputFields: Array[OutputField]

  def candidateOutputFields: Array[OutputField] = if (output != null)
    output.map(_.outputFields).getOrElse(defaultOutputFields) else Array.empty

  def outputFields: Array[OutputField] = output.map(_.finalOutputFields).getOrElse(defaultOutputFields)

  def outputNames: Array[String] = outputFields.map(_.name)

  def containInterResults: Boolean = output.map(_.containInterResults).getOrElse(false)

  def outputIndex(feature: ResultFeature, value: Option[Any] = None): Int = {
    val ofs = candidateOutputFields
    for (i <- 0 until ofs.length) {
      if (ofs(i).feature == feature && ofs(i).value == value) {
        return i
      }
    }
    -1
  }

  def isPredictionOnly: Boolean = {
    candidateOutputFields.length == 1 && candidateOutputFields(0).feature == ResultFeature.predictedValue
  }

  /** Returns targets that are residual values to be computed, the input data must include target values. */
  def targetFieldsOfResidual = targetNamesOfResidual.map(x => field(x))

  def targetNamesOfResidual = candidateOutputFields.filter(_.feature == ResultFeature.residual).
    map(x => x.targetField.getOrElse(targetName)).toSet.toArray
}

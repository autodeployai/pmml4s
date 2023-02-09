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
package org.pmml4s.metadata

import org.pmml4s.PmmlDeprecated

import scala.collection.mutable

/** Result Features */
object ResultFeature extends Enumeration {
  type ResultFeature = Value
  val predictedValue, predictedDisplayValue, transformedValue, decision, probability, affinity, residual, standardError,
  standardDeviation, clusterId, clusterAffinity, entityId, entityAffinity, warning, ruleValue, reasonCode, antecedent,
  consequent, rule, ruleId, confidence, support, lift, leverage, confidenceIntervalLower, confidenceIntervalUpper = Value
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
      new OutputField("predicted",
        Some(s"Predicted value"),
        if (model.isRegression) RealType else StringType,
        if (model.isRegression) OpType.continuous else OpType.nominal)
    }
  }

  def predictedValue(target: Field): OutputField = {
    new OutputField(s"predicted_${target.name}",
      Some(s"Predicted value of ${target.name}"),
      if (target.dataType == IntegerType && OpType.isRegression(target.opType)) RealType else target.dataType,
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
      new OutputField("probability",
        Some("Probability of predicted value"),
        RealType,
        OpType.continuous,
        ResultFeature.probability)
    } else {
      new OutputField(s"probability_$value",
        Some(s"Probability of $value"),
        RealType,
        OpType.continuous,
        ResultFeature.probability,
        value = Option(value))
    }
  }

  def confidence(): OutputField = {
    new OutputField("confidence",
      Some("Confidence of predicted value"),
      RealType,
      OpType.continuous,
      ResultFeature.confidence)
  }

  def nodeId(): OutputField = {
    entityId("node_id", "ID of hit node")
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
    new OutputField(s"reason_code_${rank}",
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

  /**
   * User-defined custom output fields, both the internal output of PMML and predefined output are ignored when the
   * field is specified.
   */
  var customOutputFields: Array[OutputField] = Array.empty

  /** A flag for whether to return those predefined output fields not exist in the output element explicitly. */
  var supplementOutput: Boolean = false

  def setSupplementOutput(value: Boolean): this.type = {
    supplementOutput = value
    this
  }

  def unionCandidateOutputFields: Array[OutputField] = output.map(x =>
    combineOutputFields(x.outputFields, defaultOutputFields)).getOrElse(defaultOutputFields)

  def unionOutputFields: Array[OutputField] = output.map(x =>
    combineOutputFields(x.finalOutputFields, defaultOutputFields)).getOrElse(defaultOutputFields)

  def output: Option[Output]

  def candidateOutputFields: Array[OutputField] = {
    if (customOutputFields != null && customOutputFields.length > 0)
      customOutputFields
    else
      output.map(x => if (supplementOutput) unionCandidateOutputFields else x.outputFields).getOrElse(defaultOutputFields)
  }

  def outputFields: Array[OutputField] = {
    if (customOutputFields != null && customOutputFields.length > 0)
      customOutputFields
    else
      output.map(x => if (supplementOutput) unionOutputFields else x.finalOutputFields).getOrElse(defaultOutputFields)
  }

  def setOutputFields(outputFields: Array[OutputField]): this.type = {
    customOutputFields = outputFields
    this
  }

  def outputNames: Array[String] = outputFields.map(_.name)

  def containInterResults: Boolean = output.map(_.containInterResults).getOrElse(false)

  def outputIndex(feature: ResultFeature, value: Option[Any] = None): Int = {
    val ofs = candidateOutputFields
    var i = 0
    while (i < ofs.length) {
      if (ofs(i).feature == feature && (feature != ResultFeature.probability || ofs(i).value == value)) {
        return i
      }
      i += 1
    }
    -1
  }

  lazy val predictedValueIndex: Int = outputIndex(ResultFeature.predictedValue)

  def isPredictionOnly: Boolean = {
    candidateOutputFields.length == 1 && candidateOutputFields(0).feature == ResultFeature.predictedValue
  }

  /** Returns targets that are residual values to be computed, the input data must include target values. */
  def targetFieldsOfResidual = targetNamesOfResidual.map(x => field(x))

  def targetNamesOfResidual = candidateOutputFields.filter(_.feature == ResultFeature.residual).
    map(x => x.targetField.getOrElse(targetName)).toSet.toArray

  def combineOutputFields(listA: Array[OutputField], listB: Array[OutputField]): Array[OutputField] = {
    val result = mutable.ArrayBuilder.make[OutputField]
    result.sizeHint(listA.size + listB.size)
    result ++= listA

    for(x <- listB) {
      var i = 0
      var exist = false
      while (i < listA.length && !exist) {
        val y = listA(i)
        if (x.feature == y.feature && y.isFinalResult) {
          exist = true
          x.feature match {
            case ResultFeature.predictedValue => {
              if (multiTargets && x.targetField != y.targetField) {
                exist = false
              }
            }
            case ResultFeature.probability => {
              if (x.value != y.value)
                exist = false
            }
            case _ =>
          }
        }
        i += 1
      }

      if (!exist) {
        result += x
      }
    }

    result.result()
  }
}


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
package org.pmml4s.model

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common.Predication.Predication
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata._
import org.pmml4s.model.BaselineMethod.BaselineMethod
import org.pmml4s.model.ReasonCodeAlgorithm.ReasonCodeAlgorithm
import org.pmml4s.transformations.{Expression, LocalTransformations}
import org.pmml4s.util.Utils

import scala.collection.{SortedSet, immutable, mutable}

/**
 * A data mining model contains a set of input fields which are used to predict a certain target value. This
 * prediction can be seen as an assessment about a prospect, a customer, or a scenario for which an outcome is
 * predicted based on historical data. In a scorecard, input fields, also referred to as characteristics (for
 * example, "age"), are broken down into attributes (for example, "19-29" and "30-39" age groups or ranges) with
 * specific partial scores associated with them. These scores represent the influence of the input attributes on the
 * target and are readily available for inspection. Partial scores are then summed up so that an overall score can be
 * obtained for the target value.
 *
 * Scorecards are very popular in the financial industry for their interpretability and ease of implementation, and
 * because input attributes can be mapped to a series of reason codes which provide explanations of each individual's
 * score. Usually, the lower the overall score produced by a scorecard, the higher the chances of it triggering an
 * adverse decision, which usually involves the referral or denial of services. Reason codes, as the name suggests,
 * allow for an explanation of scorecard behavior and any adverse decisions generated as a consequence of the overall
 * score. They basically answer the question: "Why is the score low, given its input conditions?"
 */
class Scorecard(
                 var parent: Model,
                 override val attributes: ScorecardAttributes,
                 override val miningSchema: MiningSchema,
                 val characteristics: Characteristics,
                 override val output: Option[Output] = None,
                 override val targets: Option[Targets] = None,
                 override val localTransformations: Option[LocalTransformations] = None,
                 override val modelStats: Option[ModelStats] = None,
                 override val modelExplanation: Option[ModelExplanation] = None,
                 override val modelVerification: Option[ModelVerification] = None,
                 override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedScorecardAttributes {

  val ch = characteristics.characteristics

  /** Collected all reason codes with order that appears in the PMML file, from top to bottom. */
  val reasonCodes = SortedSet(ch.flatMap(x => (x.reasonCode +: x.attributes.map(_.reasonCode))).filter(_.isDefined).toIndexedSeq: _*)

  /** The number of reason codes need to return. */
  lazy val reasonCodesWanted = outputFields.filter(_.feature == ResultFeature.reasonCode).size

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.Scorecard

  /**
   * The scoring procedure for a scorecard is simple. Partial scores are summed up to create an overall score, the
   * result of the scorecard.
   */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs()
    val results = ch.map(x => x.score(series))
    outputs.predictedValue = results.map(_._1).sum + initialScore

    if (useReasonCodes) {
      val pointsMissed = mutable.HashMap(reasonCodes.map(x => x -> 0.0).toSeq: _*)
      var i = 0
      while (i < ch.length) {
        val baseline = ch(i).baselineScore.getOrElse(baselineScore.get)
        val difference = if (reasonCodeAlgorithm == ReasonCodeAlgorithm.pointsBelow)
          baseline - results(i)._1 else results(i)._1 - baseline
        pointsMissed += results(i)._2 -> (pointsMissed(results(i)._2) + difference)
        i += 1
      }

      // if the difference between partial and baseline scores is the same for competing reason codes, the reason
      // code to be output first will be the one associated with the Attribute or Characteristic that appears first
      // in the PMML file, from top to bottom.
      val rankedReasonCodes = pointsMissed.toArray.sortWith((x1, x2) => {
        if (x1._2 == x2._2) {
          val reasonCodesSeq = reasonCodes.toSeq
          reasonCodesSeq.indexOf(x1._1) > reasonCodesSeq.indexOf(x2._1)
        } else x1._2 > x2._2
      })

      val validReasonCodes = rankedReasonCodes.filter(_._2 > 0).map(_._1.getOrElse(""))
      outputs.reasonCodes = if (validReasonCodes.length < reasonCodesWanted) {
        if (validReasonCodes.length > 0) {
          validReasonCodes ++ Array.fill(reasonCodesWanted - validReasonCodes.length)(validReasonCodes.last)
        } else Array.fill(reasonCodesWanted)(null)
      } else validReasonCodes
    }

    result(series, outputs)
  }

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override lazy val defaultOutputFields: Array[OutputField] = {
    val result = mutable.ArrayBuilder.make[OutputField]
    result += OutputField.predictedValue(this)

    if (useReasonCodes) {
      var i = 0
      while (i < reasonCodes.size) {
        result += OutputField.reasonCode(i + 1)
        i += 1
      }
    }

    result.result()
  }


  /** Creates an object of subclass of ModelOutputs that is for writing into an output series.  */
  override def createOutputs(): ScorecardOutput = new ScorecardOutput
}

/** Envelopes for all scorecard characteristics. */
class Characteristics(val characteristics: Array[Characteristic]) extends PmmlElement

/**
 * Defines the point allocation strategy for each scorecard characteristic (numeric or categorical). Once point
 * allocation between input attributes and partial scores takes place, each scorecard characteristic is assigned a
 * single partial score which is used to compute the overall score. The overall score is simply the sum of all partial
 * scores. Partial scores are assumed to be continuous values of type "double".
 *
 * @param name          Name of the characteristic. For informational reasons only.
 * @param reasonCode    Contains the characteristic's reason code, which will be later mapped to a business reason
 *                      usually
 *                      associated with an adverse decision.
 * @param baselineScore Sets the characteristic's baseline score against which to compare the actual partial score when
 *                      determining the ranking of reason codes. This attribute is required when useReasonCodes
 *                      attribute is "true" and attribute baselineScore is not defined in element Scorecard. Whenever
 *                      baselineScore is defined for a Characteristic, it takes precedence over the baselineScore
 *                      attribute value defined in element Scorecard. Note that the design-time technique used to
 *                      determine the baseline scores is captured in the baselineMethod attribute.
 * @param attributes    Input attributes for each scorecard characteristic are defined in terms of predicates.
 */
class Characteristic(val name: Option[String],
                     val reasonCode: Option[String],
                     val baselineScore: Option[Double],
                     val attributes: Array[Attribute]) extends PmmlElement {

  def score(series: Series): (Double, Option[String]) = {
    for (attribute <- attributes) {
      if (attribute.eval(series) == Predication.TRUE) {
        return (attribute.score(series), attribute.reasonCode.orElse(reasonCode))
      }
    }

    (Double.NaN, None)
  }
}


/**
 * Defines input attributes for each scorecard characteristic are defined in terms of predicates. For numeric
 * characteristics, predicates are used to implement the mapping from a range of continuous values to a partial score
 * . For example, age range 20 to 29 may map to partial score "15". For categorical characteristics, predicates are
 * used to implement the mapping of categorical values to partial scores. Note that while predicates will not
 * (typically) overlap, the Scoring Procedure requires the ordering of Attributes to be respected, and that the first
 * matching Attribute shall determine the partial scored value.
 *
 * @param reasonCode          Defines the attribute's reason code. If the reasonCode attribute is used in this level,
 *                            it takes precedence over the reasonCode attribute associated with the Characteristic
 *                            element.
 * @param partialScore        Defines the score points awarded to the Attribute. Note that attribute partialScore is
 *                            optional. A partial score is required though to be specified for every Attribute.
 *                            Either it needs to be defined through the partialScore attribute or through the
 *                            ComplexPartialScore element as defined below.
 * @param predicate           The condition upon which the mapping between input attribute and partial score takes
 *                            place. For more details on PREDICATE see the section on predicates in TreeModel for an
 *                            explanation on how predicates are described and evaluated. In scorecard models, all the
 *                            predicates defining the Attributes for a particular Characteristic must all reference a
 *                            single field.
 * @param complexPartialScore Used to implement complex point allocation of the score points awarded to the Attribute
 *                            . To be used in lieu of attribute partialScore. If both are defined, element
 *                            ComplexPartialScore takes precedence over attribute partialScore for computing the
 *                            score points awarded to the Attribute. Whenever element ComplexPartialScore is used,
 *                            the actual partial score is the value returned by the EXPRESSION (see Transformations
 *                            for more information).
 */
class Attribute(val reasonCode: Option[String],
                val partialScore: Option[Double],
                val predicate: Predicate,
                val complexPartialScore: Option[ComplexPartialScore]) extends Predicate with PmmlElement {
  require(complexPartialScore.isDefined || partialScore.isDefined,
    "A partial score is required though to be specified for every Attribute")

  /** Evaluates the predicate. */
  override def eval(series: Series): Predication = predicate.eval(series)

  def score(series: Series): Double = complexPartialScore.map(x => x.eval(series)).getOrElse(partialScore.get)
}

/**
 * Defines ComplexPartialScore, the actual partial score is the value returned by the EXPRESSION (see [[org.pmml4s
 * .transformations]]
 * for more information).
 */
class ComplexPartialScore(val expression: Expression) extends RegressionEvaluator with PmmlElement {
  override def eval(series: Series): Double = Utils.toDouble(expression.eval(series))
}


/** Describes how reason codes shall be ranked. */
object ReasonCodeAlgorithm extends Enumeration {
  type ReasonCodeAlgorithm = Value
  val pointsAbove, pointsBelow = Value
}

/**
 * An informational string describing the technique used by the model designer to establish the baseline scores. Allowed
 * values are:
 * - max: Indicates that baseline scores are the maximum partial score in element Characteristic
 * - min: Baseline scores are the minimum partial score in Characteristic
 * - mean: Baseline scores are the mean (weighted average) partial score in Characteristic
 * - neutral: Baseline scores are the risk-neutral partial score in Characteristic
 * - other: Baseline scores are derived using any other technique.
 *
 * This attribute is purely informational and does not influence the runtime calculations of reason codes. (By contrast,
 * the reasonCodeAlgorithm is critical to achieving an accurate calculation of reasons.)
 */
object BaselineMethod extends Enumeration {
  type BaselineMethod = Value
  val max, min, mean, neutral, other = Value
}

trait HasScorecardAttributes extends HasModelAttributes {
  /**
   * Initial score contains a value which is added to the overall score whenever partial scores are summed up.
   */
  def initialScore: Double

  /** If "false", reason codes are not computed as part of the scorecard. */
  def useReasonCodes: Boolean

  /**
   * May be "pointsAbove" or "pointsBelow", describing how reason codes shall be ranked, relative to the baseline score
   * of each Characteristic, or as set at the top-level scorecard.
   */
  def reasonCodeAlgorithm: ReasonCodeAlgorithm

  /**
   * A single value to use as the baseline comparison score for all characteristics, when determining reason code
   * ranking. Alternatively, unique baseline scores may be set for each individual Characteristic as shown below. This
   * attribute is required only when useReasonCodes is "true" and attribute baselineScore is not given for each
   * Characteristic.
   */
  def baselineScore: Option[Double]

  /**
   * An informational string describing the technique used by the model designer to establish the baseline scores.
   */
  def baselineMethod: BaselineMethod
}


/**
 * Holds attributes of a Scorecard.
 */
class ScorecardAttributes(
                           override val functionName: MiningFunction = MiningFunction.regression,
                           override val modelName: Option[String] = None,
                           override val algorithmName: Option[String] = None,
                           override val isScorable: Boolean = true,
                           val initialScore: Double = 0.0,
                           val useReasonCodes: Boolean = true,
                           val reasonCodeAlgorithm: ReasonCodeAlgorithm = ReasonCodeAlgorithm.pointsBelow,
                           val baselineScore: Option[Double] = None,
                           val baselineMethod: BaselineMethod = BaselineMethod.other)
  extends ModelAttributes(functionName, modelName, algorithmName, isScorable) with HasScorecardAttributes

trait HasWrappedScorecardAttributes extends HasWrappedModelAttributes with HasScorecardAttributes {

  override def attributes: ScorecardAttributes

  def initialScore: Double = attributes.initialScore

  def useReasonCodes: Boolean = attributes.useReasonCodes

  def reasonCodeAlgorithm: ReasonCodeAlgorithm = attributes.reasonCodeAlgorithm

  def baselineScore: Option[Double] = attributes.baselineScore

  def baselineMethod: BaselineMethod = attributes.baselineMethod
}

class ScorecardOutput extends RegOutputs with MutableReasonCodes


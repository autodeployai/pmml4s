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

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata.{Field, MiningSchema, Output, Targets}
import org.pmml4s.transformations.{DerivedField, LocalTransformations}
import org.pmml4s.util.Utils

import scala.collection.immutable

/**
 * Naïve Bayes uses Bayes' Theorem, combined with a ("naive") presumption of conditional independence, to predict the
 * value of a target (output), from evidence given by one or more predictor (input) fields.
 *
 * Naïve Bayes models require the target field to be discretized so that a finite number of values are considered by
 * the model.
 */
class NaiveBayesModel(
                       override var parent: Model,
                       override val attributes: NaiveBayesAttributes,
                       override val miningSchema: MiningSchema,
                       val bayesInputs: BayesInputs,
                       val bayesOutput: BayesOutput,
                       override val output: Option[Output] = None,
                       override val targets: Option[Targets] = None,
                       override val localTransformations: Option[LocalTransformations] = None,
                       override val modelStats: Option[ModelStats] = None,
                       override val modelExplanation: Option[ModelExplanation] = None,
                       override val modelVerification: Option[ModelVerification] = None,
                       override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedNaiveBayesAttributes {

  bayesInputs.inputs.foreach(_.init(targetField, threshold))
  private val targetCounts: Array[Double] = classes.map(x => math.log(bayesOutput.targetValueCounts.countOf(x)))


  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.NaiveBayesModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val probLog = targetCounts.clone()
    for (bayesInput <- bayesInputs.inputs) {
      val p = bayesInput.eval(series, threshold)
      for (i <- 0 until p.length) {
        // using log-sum-exp trick to fix underflow and overflow
        probLog(i) += math.log(p(i))
      }
    }

    val max = probLog.max
    val probExp = probLog.map(x => math.exp(x - max))
    val sum = probExp.sum

    val outputs = createOutputs().setProbabilities(classes.zip(probExp.map(_ / sum)).toMap)
    outputs.evalPredictedValueByProbabilities()

    result(series, outputs)
  }

  /** Creates an object of NaiveBayesOutputs that is for writing into an output series.  */
  override def createOutputs(): NaiveBayesOutputs = new NaiveBayesOutputs
}

/** Contains several BayesInput elements. */
class BayesInputs(val inputs: Array[BayesInput]) extends PmmlElement

/**
 * For a discrete field, each BayesInput contains the counts pairing the discrete values of that field with those of the
 * target field.
 * For a continuous field, the BayesInput element lists the distributions obtained for that field with each value of the
 * target field. BayesInput may also be used to define how continuous values are encoded as discrete bins.
 * (Discretization is achieved using DerivedField; only the Discretize mapping for DerivedField may be invoked here).
 *
 * Note that a BayesInput element encompasses either one TargetValueStats element or one or more PairCounts elements.
 * Element DerivedField can only be used in conjunction with PairCounts.
 */
class BayesInput(val fieldName: Field,
                 val targetValueStats: Option[TargetValueStats],
                 val pairCounts: Array[PairCounts],
                 val derivedField: Option[DerivedField] = None) extends PmmlElement {

  private var distributions: Array[ContinuousDistribution] = null
  private var probabilities: Array[Array[Double]] = null

  def init(target: Field, threshold: Double): Unit = {
    val classes = target.validValues
    val l = classes.length
    if (targetValueStats.isDefined) {
      distributions = Array.ofDim(l)
      for (i <- 0 until l) {
        distributions(i) = targetValueStats.get.getDist(classes(i)).get
      }
    } else {
      val f = derivedField.getOrElse(fieldName)
      val n = derivedField.map(_.numCategories).getOrElse(fieldName.numCategories)
      val counts = Array.ofDim[Double](l)
      probabilities = Array.ofDim(n, l)

      for (pair <- pairCounts) {
        val i = f.encode(pair.value)
        for (c <- pair.targetValueCounts.targetValueCounts) {
          val j = target.encode(c.value)
          probabilities(i.toInt)(j.toInt) = c.count
          counts(j.toInt) += c.count
        }
      }

      for (i <- 0 until n) {
        for (j <- 0 until l) {
          if (probabilities(i)(j) > 0) {
            probabilities(i)(j) /= counts(j)
          } else {
            probabilities(i)(j) = threshold
          }
        }
      }
    }
  }

  def eval(series: Series, threshold: Double): Array[Double] = {
    if (series.isMissingAt(fieldName.index)) {
      Array.emptyDoubleArray
    } else if (distributions != null) {
      distributions.map(x => Math.max(threshold, x.probability(Utils.toDouble(series.get(fieldName.index)))))
    } else {
      val v = derivedField.map(x => x.encode(x.eval(series))).getOrElse(fieldName.encode(series))
      probabilities(v.toInt)
    }
  }
}


/** Serves as the envelope for element TargetValueStat. */
class TargetValueStats(val targetValueStats: Array[TargetValueStat]) extends PmmlElement {
  private val map = targetValueStats.map(x => (x.value, x.distribution)).toMap

  def getDist(x: Any): Option[ContinuousDistribution] = map.get(x)
}

/**
 * PairCounts lists, for a field Ii's discrete value Iij, the TargetValueCounts that pair the value Iij with each value
 * of the target field.
 */
class PairCounts(val value: Any, val targetValueCounts: TargetValueCounts) extends PmmlElement

/**
 * Used for a continuous input field Ii to define statistical measures associated with each value of the target field.
 * As defined in CONTINUOUS-DISTRIBUTION-TYPES, different distribution types can be used to represent such measures.
 * For Bayes models, these are restricted to Gaussian and Poisson distributions.
 */
class TargetValueStat(val value: Any, val distribution: ContinuousDistribution) extends PmmlElement {
  require(distribution.distType == ContinuousDistributionType.GAUSSIAN ||
    distribution.distType == ContinuousDistributionType.POISSON,
    s"Both Gaussian and Poisson distributions are only available for Bayes models, but got ${distribution}")
}

/** Contains the counts associated with the values of the target field. */
class BayesOutput(val fieldName: Field, val targetValueCounts: TargetValueCounts) extends PmmlElement

/**
 * Lists the counts associated with each value of the target field, However, a TargetValueCount whose count is zero may
 * be omitted.
 * Within BayesOutput, TargetValueCounts lists the total count of occurrences of each target value.
 * Within PairCounts, TargetValueCounts lists, for each target value, the count of the joint occurrences of that target
 * value with a particular discrete input value.
 */
class TargetValueCounts(val targetValueCounts: Array[TargetValueCount]) extends PmmlElement {
  private val map = targetValueCounts.map(x => (x.value, x.count)).toMap

  def countOf(value: Any): Double = map.get(value).getOrElse(0.0)
}

class TargetValueCount(val value: Any, val count: Double) extends PmmlElement

trait HasNaiveBayesAttributes extends HasModelAttributes {

  /**
   * Specifies a default (usually very small) probability to use in lieu of P(Ij* | Tk) when count[Ij*Ti] is zero.
   * Similarly, since the probabilily of a continuous distribution can reach the value of 0 as the lower limit, the
   * same threshold parameter is used as the probability of the continuous variable when the calculated probability of
   * the distribution falls below that value.
   */
  def threshold: Double
}

trait HasWrappedNaiveBayesAttributes extends HasWrappedModelAttributes with HasNaiveBayesAttributes {

  override def attributes: NaiveBayesAttributes

  def threshold: Double = attributes.threshold
}

class NaiveBayesAttributes(
                            override val threshold: Double,
                            override val functionName: MiningFunction,
                            override val modelName: Option[String] = None,
                            override val algorithmName: Option[String] = None,
                            override val isScorable: Boolean = true
                          ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable)
  with HasNaiveBayesAttributes

class NaiveBayesOutputs extends ClsOutputs
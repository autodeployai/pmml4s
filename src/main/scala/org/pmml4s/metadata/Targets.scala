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

import org.pmml4s.common.{OpType, PmmlElement}
import org.pmml4s.model.Model

/**
 * If a regression model should predict integers, use the attribute castInteger to control how decimal places should be
 * handled.
 */
object CastInteger extends Enumeration {
  type CastInteger = Value

  /**
   * round: round to nearest integer, e.g., 2.718 becomes 3, -2.89 becomes -3
   * ceiling: smallest integer greater than or equal, e.g., 2.718 becomes 3, -1.2 becomes -1
   * floor: largest integer smaller than or equal, e.g., 2.718 becomes 2, -1.2 becomes -2
   */
  val round, ceiling, floor = Value
}

import org.pmml4s.metadata.CastInteger._

/**
 * @param value            corresponds to the class labels in a classification model.
 * @param displayValue     usually more readable version which can be used by PMML consumers to display values in
 *                         scoring results or other applications.
 * @param priorProbability specifies a default probability for the corresponding target category. It is used if the
 *                         prediction logic itself did not produce a result.
 *                         The attribute priorProbability is used only if the optype of the field is categorical or
 *                         ordinal.
 * @param defaultValue     the counterpart of prior probabilities for continuous fields. Usually the value is the mean
 *                         of the target values in the training data. The attribute defaultValue is used only if the
 *                         optype of the field is continuous.
 */
class TargetValue(
                   val value: Option[Any],
                   val displayValue: Option[String],
                   val priorProbability: Option[Double],
                   val defaultValue: Option[Double]) extends PmmlElement

/**
 * Note that castInteger, min, max, rescaleConstant and rescaleFactor only apply to models of type regression.
 * Furthermore, they must be applied in sequence, which is:
 *
 * min and max
 * rescaleFactor
 * rescaleConstant
 * castInteger
 *
 * @param field           must refer to a name of a DataField or DerivedField. It can be absent when the model is used
 *                        inside a Segment of a MiningModel and does not have a real target field in the input data
 * @param optype          When Target specifies optype then it overrides the optype attribute in a corresponding
 *                        MiningField, if it exists. If the target does not specify optype then the MiningField is used
 *                        as default. And, in turn, if the MiningField does not specify an optype, it is taken from the
 *                        corresponding DataField. In other words, a MiningField overrides a DataField, and a Target
 *                        overrides a MiningField.
 * @param castInteger     If a regression model should predict integers, use the attribute castInteger to control how
 *                        decimal places should be handled.
 * @param min             If min is present, the predicted value will be the value of min if it is smaller than that.
 * @param max             If max is present, the predicted value will be max if it is larger than that.
 * @param rescaleConstant can be used for simple rescale of the predicted value: First off, the predicted value is
 *                        multiplied by rescaleFactor.
 * @param rescaleFactor   after that, rescaleConstant is added to the predicted value.
 * @param targetValues    In classification models, TargetValue is required. For regression models, TargetValue is only
 *                        optional.
 */
class Target(
              val field: Option[String],
              val optype: Option[OpType],
              val castInteger: Option[CastInteger],
              val min: Option[Double],
              val max: Option[Double],
              val rescaleConstant: Double = 0.0,
              val rescaleFactor: Double = 1.0,
              val targetValues: Array[TargetValue] = Array.empty) extends PmmlElement {

  lazy val priorPredictedValue: Any = if (priorProbabilities.nonEmpty) priorProbabilities.maxBy(_._2)._1 else null
  lazy val displayValues: Map[Any, String] =
    targetValues.filter(x => (x.value.isDefined && x.displayValue.isDefined)).map(x => x.value.get -> x.displayValue.get).toMap

  def defaultValue: Option[Double] = if (targetValues.length != 0) targetValues(0).defaultValue else None

  def categories: Array[Any] = targetValues.flatMap(_.value)

  def priorProbabilities: Map[Any, Double] = {
    targetValues.map(x => (x.value, x.priorProbability)).filter(_._2.isDefined).map(x => (x._1.orNull, x._2.get)).toMap
  }

  def postPredictedValue(predictedValue: Double): Double = {
    val a = if (min.isDefined) Math.max(min.get, predictedValue) else predictedValue
    val b = if (max.isDefined) Math.min(max.get, a) else a
    val c = b * rescaleFactor + rescaleConstant

    castInteger.map(x => x match {
      case `round`   => Math.round(c)
      case `ceiling` => Math.ceil(c)
      case `floor`   => Math.floor(c)
    }).getOrElse(c)
  }

  def displayValue(value: Any): Option[String] = displayValues.get(value)
}

trait HasTargetFields {
  def targetNames: Array[String]

  /** Name of the first target for the supervised model. */
  def targetName: String = if (targetNames.nonEmpty) targetNames.head else null

  def hasTarget: Boolean = targetNames.nonEmpty

  def multiTargets: Boolean = targetNames.length > 1

  def singleTarget: Boolean = targetNames.length == 1

  def size: Int = targetName.length
}

class Targets(val targets: Array[Target]) extends HasTargetFields with PmmlElement {
  private lazy val map: Map[String, Target] = targetNames.zip(targets).toMap

  override def targetNames: Array[String] = targets.map(_.field.getOrElse(""))

  def get(name: String): Option[Target] = map.get(name)

  def apply(name: String): Target = map(name)

  def target: Target = targets.head

  def categories: Array[Any] = target.categories

  def categories(name: String): Option[Array[Any]] = get(name).map(_.categories)

  def postPredictedValue(predictedValue: Double): Double = target.postPredictedValue(predictedValue)

  def defaultValue: Option[Double] = target.defaultValue

  def priorPredictedValue: Any = target.priorPredictedValue

  def priorPredictedValue(name: String): Any = get(name).map(_.priorPredictedValue).orNull

  def priorProbabilities: Map[Any, Double] = target.priorProbabilities

  def priorProbabilities(name: String): Map[Any, Double] = get(name).map(_.priorProbabilities).getOrElse(Map.empty)

  def displayValue(value: Any, name: String = null): Option[String] = if (name eq null) target.displayValue(value) else
    get(name).flatMap(x => x.displayValue(name))
}

trait HasTargets {
  self: Model =>
  def targets: Option[Targets]
}


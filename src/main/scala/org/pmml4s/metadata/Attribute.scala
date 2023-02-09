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

import org.pmml4s.common._
import org.pmml4s.metadata.AttributeType.{Categorical, Continuous, Typeless}
import org.pmml4s.util.Utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait HasLabels {
  def labels: Map[Any, String]

  def getLabel(value: Any): Option[String] = labels.get(value)
}

trait HasMissingValues {
  def missingValues: Set[Any]

  def isMissingValue(value: Any): Boolean = isSysMissing(value) || missingValues.contains(value)

  def isSysMissing(value: Any): Boolean = Utils.isMissing(value)
}

trait HasInvalidValues {
  def invalidValues: Set[Any]

  def isInvalidValue(value: Any): Boolean = invalidValues.contains(value)
}

trait HasValidValues {
  self: Attribute =>
  def validValues: Array[Any]

  def isValidValue(value: Any): Boolean = !isMissingValue(value) && !isInvalidValue(value)

  def numCategories: Int = validValues.length

  /**
   * Tests whether this field is binary, that is categorical and it
   * contains exactly two valid values.
   */
  def isBinary: Boolean = false
}

trait ValueIndexer {
  def encode(value: Any): Double

  def decode(index: Int): Any
}

trait MutableValueIndexer extends ValueIndexer

sealed trait AttributeType

object AttributeType {
  /**
   * Defines const variables could be used in Java.
   * Use Object.XXX() instead of Object.xxx$.MODULE$
   */
  val CONTINUOUS = Continuous
  val CATEGORICAL = Categorical
  val TYPELESS = Typeless

  case object Continuous extends AttributeType

  case object Categorical extends AttributeType

  case object Typeless extends AttributeType

}

trait Attribute extends HasLabels
  with HasMissingValues
  with HasInvalidValues
  with HasValidValues
  with HasIntervals
  with ValueIndexer
  with Serializable {

  /** Attribute type. */
  def attrType: AttributeType

  def toAttribute: Attribute = this

  def isMutable: Boolean = false

  def valuesAsString: String = attrType match {
    case Continuous  => intervals.map(_.toString).mkString(",")
    case Categorical => validValues.map(_.toString).mkString(if (isBinary) "/" else ",")
    case Typeless    => ""
  }
}

object Attribute {

  import org.pmml4s.common.OpType._

  def apply(dataType: DataType, opType: OpType, intervals: Array[Interval], values: Array[Value]): Attribute = {
    val (validVals, invalidVals, missingVals, labels) = Value.distinguish(values, dataType)
    apply(opType, intervals, validVals, invalidVals, missingVals, labels)
  }

  def apply(opType: OpType,
            intervals: Array[Interval],
            validVals: Array[Any],
            invalidVals: Set[Any],
            missingVals: Set[Any],
            labels: Map[Any, String]): Attribute = opType match {
    case `nominal`    => CategoricalAttribute(validVals, invalidVals, missingVals, labels)
    case `ordinal`    => CategoricalAttribute(validVals, invalidVals, missingVals, labels)
    case `continuous` => ContinuousAttribute(intervals, validVals, invalidVals, missingVals, labels)
    case `typeless`   => TypelessAttribute
  }


  def apply(opType: OpType): Attribute = opType match {
    case `nominal`    => new MutableCategoricalAttribute
    case `ordinal`    => new MutableCategoricalAttribute
    case `continuous` => new ContinuousAttribute
    case `typeless`   => TypelessAttribute
  }

}

trait MutableAttribute extends Attribute with MutableValueIndexer {
  override def isMutable: Boolean = true
}

class ContinuousAttribute(
                           override val intervals: Array[Interval] = Array.empty,
                           override val validValues: Array[Any] = Array.empty,
                           override val invalidValues: Set[Any] = Set.empty,
                           override val missingValues: Set[Any] = Set.empty,
                           override val labels: Map[Any, String] = Map.empty) extends Attribute with HasIntervals {

  private val set: Set[Any] = validValues.toSet

  def attrType: AttributeType = AttributeType.Continuous

  override def isValidValue(value: Any): Boolean = if (validValues.length > 0) set.contains(value) else super.isValidValue(value)

  override def isInvalidValue(value: Any): Boolean = super.isInvalidValue(value) || (!isMissingValue(value) && (if (validValues.length > 0) !set.contains(value) else !isIn(value)))

  // We don't need to test if the specified value is invalid, because the invalid value has been handled by the preprocess operation.
  // In the predicting phase, there are only both types: missing and valid.
  def encode(value: Any): Double = if (!isMissingValue(value)) Utils.toDouble(value) else Double.NaN

  def decode(index: Int): Any = throw new UnsupportedOperationException("decode is not supported by the continuous field")
}

object ContinuousAttribute {

  def apply(interval: Interval, missingVals: Set[Any]=Set.empty): ContinuousAttribute =
    new ContinuousAttribute(Array(interval), missingValues=missingVals)

  def apply(intervals: Array[Interval], values: Array[Any], invalidVals: Set[Any], missingVals: Set[Any], labels: Map[Any, String]) =
    new ContinuousAttribute(intervals, values, invalidVals, missingVals, labels)
}

abstract class CategoricalAttribute(
                                     override val invalidValues: Set[Any] = Set.empty,
                                     override val missingValues: Set[Any] = Set.empty,
                                     override val labels: Map[Any, String] = Map.empty) extends Attribute {
  def attrType: AttributeType = AttributeType.Categorical

  override def isBinary: Boolean = numCategories == 2

  override def intervals: Array[Interval] = Array.empty
}

object CategoricalAttribute {
  def apply(values: Array[Any], invalidVals: Set[Any], missingVals: Set[Any], labels: Map[Any, String]): CategoricalAttribute =
    if (values.length == 0)
      new MutableCategoricalAttribute(invalidVals, missingVals, labels)
    else
      new ImmutableCategoricalAttribute(values, invalidVals, missingVals, labels)
}

class ImmutableCategoricalAttribute(
                                     override val validValues: Array[Any],
                                     override val invalidValues: Set[Any] = Set.empty,
                                     override val missingValues: Set[Any] = Set.empty,
                                     override val labels: Map[Any, String] = Map.empty
                                   ) extends CategoricalAttribute() {
  private val map: Map[Any, Double] = validValues.zipWithIndex.map(x => (x._1, x._2.toDouble)).toMap

  override def isValidValue(value: Any): Boolean = {
    if (map.nonEmpty)
      !encode(value).isNaN
    else
      super.isValidValue(value)
  }

  override def isInvalidValue(value: Any): Boolean = {
    super.isInvalidValue(value) || (!isMissingValue(value) && (map.nonEmpty && !map.contains(value)))
  }

  def encode(value: Any): Double = if (map.nonEmpty) map.get(value).getOrElse(Double.NaN) else {
    if (isValidValue(value)) Utils.toDouble(value) else Double.NaN
  }

  def decode(index: Int): Any = if (map.nonEmpty) validValues(index) else
    throw new UnsupportedOperationException("decode is not supported by the categorical field without values specified")
}

class MutableCategoricalAttribute(
                                   override val invalidValues: Set[Any] = Set.empty,
                                   override val missingValues: Set[Any] = Set.empty,
                                   override val labels: Map[Any, String] = Map.empty) extends CategoricalAttribute(invalidValues, missingValues, labels) {

  private val valsMapBuffer: mutable.Map[Any, Double] = new mutable.HashMap[Any, Double]
  private val values: ArrayBuffer[Any] = new ArrayBuffer[Any]

  override def encode(value: Any): Double = if (valsMapBuffer.contains(value)) valsMapBuffer(value) else {
    if (isValidValue(value)) {
      val index = validValues.length.toDouble
      values += value
      valsMapBuffer.put(value, index)
      index
    } else Double.NaN
  }

  def decode(index: Int): Any = if (values.nonEmpty) values(index) else
    throw new UnsupportedOperationException("decode is not supported by the categorical field without values specified")

  override def isMutable: Boolean = true

  override def toAttribute: Attribute = this

  override def validValues: Array[Any] = values.toArray
}

object TypelessAttribute extends Attribute {

  override def validValues: Array[Any] = Array.empty[Any]

  override def invalidValues: Set[Any] = Set.empty

  override def missingValues: Set[Any] = Set.empty

  override def intervals: Array[Interval] = Array.empty

  override def labels: Map[Any, String] = Map.empty

  def encode(value: Any): Double = Double.NaN

  def decode(index: Int): Any = throw new UnsupportedOperationException("decode is not supported by the typeless field")

  def attrType: AttributeType = AttributeType.Typeless
}


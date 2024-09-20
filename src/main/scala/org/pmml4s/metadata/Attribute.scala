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
import org.pmml4s.data.DataVal
import org.pmml4s.metadata.AttributeType.{Categorical, Continuous, Typeless}
import org.pmml4s.util.Utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait HasLabels {
  def labels: Map[DataVal, String]

  def getLabel(value: DataVal): Option[String] = labels.get(value)
}

trait HasMissingValues {
  def missingValues: Set[DataVal]

  @inline
  def isMissingValue(value: DataVal): Boolean =
    value == null || value.isMissing || missingValues.contains(value)

  def isMissingValue(value: Any): Boolean = isMissingValue(DataVal.from(value))
}

trait HasInvalidValues {
  def invalidValues: Set[DataVal]

  def isInvalidValue(value: DataVal): Boolean = invalidValues.contains(value)

  def isInvalidValue(value: Any): Boolean = isInvalidValue(DataVal.from(value))
}

trait HasValidValues {
  self: Attribute =>
  def validValues: Array[DataVal]

  def isValidValue(value: DataVal): Boolean = !isMissingValue(value) && !isInvalidValue(value)

  def isValidValue(value: Any): Boolean = isValidValue(DataVal.from(value))

  def numCategories: Int = validValues.length

  /**
   * Tests whether this field is binary, that is categorical and it
   * contains exactly two valid values.
   */
  def isBinary: Boolean = false
}

trait ValueIndexer {
  def encode(value: DataVal): Double

  def encode(value: Any): Double = encode(DataVal.from(value))

  def decode(index: Int): DataVal
}

trait MutableValueIndexer extends ValueIndexer

sealed trait AttributeType

object AttributeType {
  /**
   * Defines const variables could be used in Java.
   * Use Object.XXX() instead of Object.xxx$.MODULE$
   */
  val CONTINUOUS: Continuous.type = Continuous
  val CATEGORICAL: Categorical.type = Categorical
  val TYPELESS: Typeless.type = Typeless

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

  // A single numeric field without any extra attributes defined.
  def isPlain: Boolean = attrType == AttributeType.Continuous &&
    missingValues.isEmpty &&
    invalidValues.isEmpty &&
    validValues.length == 0 &&
    intervals.length == 0
}

object Attribute {

  import org.pmml4s.common.OpType._

  def apply(dataType: DataType, opType: OpType, intervals: Array[Interval], values: Array[Value]): Attribute = {
    val (validVals, invalidVals, missingVals, labels) = Value.distinguish(values, dataType)
    apply(opType, intervals, validVals, invalidVals, missingVals, labels)
  }

  def apply(opType: OpType,
            intervals: Array[Interval],
            validVals: Array[DataVal],
            invalidVals: Set[DataVal],
            missingVals: Set[DataVal],
            labels: Map[DataVal, String]): Attribute = opType match {
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
                           override val validValues: Array[DataVal] = Array.empty,
                           override val invalidValues: Set[DataVal] = Set.empty,
                           override val missingValues: Set[DataVal] = Set.empty,
                           override val labels: Map[DataVal, String] = Map.empty) extends Attribute with HasIntervals {

  private val set: Set[DataVal] = validValues.toSet

  def attrType: AttributeType = AttributeType.Continuous

  override def isValidValue(value: DataVal): Boolean = if (validValues.length > 0) set.contains(value) else super.isValidValue(value)

  override def isInvalidValue(value: DataVal): Boolean = super.isInvalidValue(value) || (!isMissingValue(value) && (if (validValues.length > 0) !set.contains(value) else !isIn(value)))

  // We don't need to test if the specified value is invalid, because the invalid value has been handled by the preprocess operation.
  // In the predicting phase, there are only both types: missing and valid.
  def encode(value: DataVal): Double = if (!isMissingValue(value)) value.toDouble else Double.NaN

  def decode(index: Int): DataVal = throw new UnsupportedOperationException("decode is not supported by the continuous field")
}

object ContinuousAttribute {

  def apply(interval: Interval, missingVals: Set[DataVal]=Set.empty): ContinuousAttribute =
    new ContinuousAttribute(Array(interval), missingValues=missingVals)

  def apply(intervals: Array[Interval], values: Array[DataVal], invalidVals: Set[DataVal], missingVals: Set[DataVal], labels: Map[DataVal, String]) =
    new ContinuousAttribute(intervals, values, invalidVals, missingVals, labels)
}

abstract class CategoricalAttribute(
                                     override val invalidValues: Set[DataVal] = Set.empty,
                                     override val missingValues: Set[DataVal] = Set.empty,
                                     override val labels: Map[DataVal, String] = Map.empty) extends Attribute {
  def attrType: AttributeType = AttributeType.Categorical

  override def isBinary: Boolean = numCategories == 2

  override def intervals: Array[Interval] = Array.empty
}

object CategoricalAttribute {
  def apply(values: Array[DataVal], invalidVals: Set[DataVal], missingVals: Set[DataVal], labels: Map[DataVal, String]): CategoricalAttribute =
    if (values.length == 0)
      new MutableCategoricalAttribute(invalidVals, missingVals, labels)
    else
      new ImmutableCategoricalAttribute(values, invalidVals, missingVals, labels)
}

class ImmutableCategoricalAttribute(
                                     override val validValues: Array[DataVal],
                                     override val invalidValues: Set[DataVal] = Set.empty,
                                     override val missingValues: Set[DataVal] = Set.empty,
                                     override val labels: Map[DataVal, String] = Map.empty
                                   ) extends CategoricalAttribute() {
  private val map: Map[DataVal, Double] = validValues.zipWithIndex.map(x => (x._1, x._2.toDouble)).toMap

  override def isValidValue(value: DataVal): Boolean = {
    if (map.nonEmpty)
      !encode(value).isNaN
    else
      super.isValidValue(value)
  }

  override def isInvalidValue(value: DataVal): Boolean = {
    super.isInvalidValue(value) || (!isMissingValue(value) && (map.nonEmpty && !map.contains(value)))
  }

  def encode(value: DataVal): Double = if (map.nonEmpty) {
    map.getOrElse(value, Double.NaN)
  } else {
    if (isValidValue(value)) Utils.toDouble(value) else Double.NaN
  }

  def decode(index: Int): DataVal = if (map.nonEmpty) validValues(index) else
    throw new UnsupportedOperationException("decode is not supported by the categorical field without values specified")
}

class MutableCategoricalAttribute(
                                   override val invalidValues: Set[DataVal] = Set.empty,
                                   override val missingValues: Set[DataVal] = Set.empty,
                                   override val labels: Map[DataVal, String] = Map.empty) extends CategoricalAttribute(invalidValues, missingValues, labels) {

  private val valsMapBuffer: mutable.Map[DataVal, Double] = new mutable.HashMap[DataVal, Double]
  private val values: ArrayBuffer[DataVal] = new ArrayBuffer[DataVal]

  override def encode(value: DataVal): Double = if (valsMapBuffer.contains(value)) valsMapBuffer(value) else {
    if (isValidValue(value)) {
      val index = validValues.length.toDouble
      values += value
      valsMapBuffer.put(value, index)
      index
    } else Double.NaN
  }

  def decode(index: Int): DataVal = if (values.nonEmpty) values(index) else
    throw new UnsupportedOperationException("decode is not supported by the categorical field without values specified")

  override def isMutable: Boolean = true

  override def toAttribute: Attribute = this

  override def validValues: Array[DataVal] = values.toArray
}

object TypelessAttribute extends Attribute {

  override def validValues: Array[DataVal] = Array.empty[DataVal]

  override def invalidValues: Set[DataVal] = Set.empty

  override def missingValues: Set[DataVal] = Set.empty

  override def intervals: Array[Interval] = Array.empty

  override def labels: Map[DataVal, String] = Map.empty

  def encode(value: DataVal): Double = Double.NaN

  def decode(index: Int): DataVal = throw new UnsupportedOperationException("decode is not supported by the typeless field")

  def attrType: AttributeType = AttributeType.Typeless
}


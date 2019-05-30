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

import org.pmml4s.FieldNotFoundException
import org.pmml4s.common.{HasDataType, HasOpType, Interval}
import org.pmml4s.data.Series
import org.pmml4s.util.Utils

import scala.collection.mutable

/**
 * Abstract class for field in a PMML.
 */
abstract class Field extends HasDataType with HasOpType with Attribute {

  require(!isDataField || name.nonEmpty, "Cannot have an empty string for name.")

  /** Name of the field. */
  def name: String

  /** Display name of the field. None if it is not set. */
  def displayName: Option[String] = None

  /** Attribute of the field. */
  def attribute: Attribute = TypelessAttribute

  /** Index of the field in the input series. */
  def index: Int = -1

  /** Sets the index of this field. */
  def index_=(i: Int)

  /** Tests if the index of this field is defined */
  def indexDefined: Boolean = index >= 0

  /** Field type. */
  def fieldType: FieldType

  /** Tests if the field is a data field. */
  def isDataField: Boolean = fieldType == FieldType.DataField

  /** Tests if the field is a derived field. */
  def isDerivedField: Boolean = fieldType == FieldType.DerivedField

  /**
   * Converts a string to the corresponding value based on its data type.
   *
   * @throws java.lang.NumberFormatException - If the string does not contain a parsable number if dataType is numeric
   */
  def toVal(s: String): Any = Utils.toVal(s, dataType)

  /**
   * Converts a string to the corresponding value based on its data type.
   *
   * @return None if any error occurs
   */
  def toValOption(s: String): Option[Any] = try {
    Option(Utils.toVal(s, dataType))
  } catch {
    case _: Exception => None
  }

  /** Retrieve its value from the specified series, return null if missing */
  def get(series: Series): Any

  /** Tests if its value is missing from the specified series. */
  def isMissing(series: Series): Boolean = Utils.isMissing(get(series))

  /** Encodes the value of the field in the input series. */
  def encode(series: Series): Double = {
    val value = get(series)
    if (value != null) encode(value) else Double.NaN
  }

  /** Retrieve its value as double from the specified series, return Double.NaN if missing. */
  def getDouble(series: Series): Double = {
    val value = get(series)
    if (value != null) Utils.toDouble(value) else Double.NaN
  }

  /** Converts to an immutable attribute if it's mutable. */
  def toImmutable(): Field = this

  /** Tests if the field is referenced in the model element. */
  def referenced: Boolean = false

  /** Sets the referenced flag of the field. */
  def referenced_=(r: Boolean)

  override def labels: Map[Any, String] = attribute.labels

  override def getLabel(value: Any): Option[String] = attribute.getLabel(value)

  override def missingValues: Set[Any] = attribute.missingValues

  override def isMissingValue(value: Any): Boolean = attribute.isMissingValue(value)

  override def invalidValues: Set[Any] = attribute.invalidValues

  override def isInvalidValue(value: Any): Boolean = attribute.isInvalidValue(value)

  override def validValues: Array[Any] = attribute.validValues

  override def isValidValue(value: Any): Boolean = attribute.isValidValue(value)

  override def intervals: Seq[Interval] = attribute.intervals

  override def numCategories: Int = attribute.numCategories

  override def isBinary: Boolean = attribute.isBinary

  override def encode(value: Any): Double = attribute.encode(value)

  override def decode(index: Int): Any = attribute.decode(index)

  override def attrType: AttributeType = attribute.attrType

  override def toAttribute: Attribute = attribute.toAttribute

  override def isMutable: Boolean = attribute.isMutable
}

trait HasField {
  /**
   * Returns the field of a given name.
   *
   * @throws FieldNotFoundException if a field with the given name does not exist
   */
  def field(name: String): Field = getField(name).getOrElse(throw new FieldNotFoundException(name))

  /**
   * Returns the field of a given name, None if a field with the given name does not exist
   */
  def getField(name: String): Option[Field]
}

trait FieldScope extends HasField

class MutableFieldScope[T <: Field] extends FieldScope {
  private val nameToField: mutable.Map[String, T] = new mutable.HashMap[String, T]()

  override def getField(name: String): Option[Field] = nameToField.get(name)

  def +=(f: T): T = {
    nameToField += f.name -> f
    f
  }
}

trait HasFieldScope {
  def scope: FieldScope
}

sealed trait FieldType

object FieldType {

  case object DataField extends FieldType

  case object DerivedField extends FieldType

  case object OutputField extends FieldType

  case object ResultField extends FieldType

  case object ParameterField extends FieldType

  case object WrappedField extends FieldType

}


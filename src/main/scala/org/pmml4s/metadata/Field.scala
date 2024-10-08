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
import org.pmml4s.common.{DoubleType, HasDataType, HasOpType, Interval}
import org.pmml4s.data.{DataVal, DoubleVal, Series}
import org.pmml4s.util.Utils

import scala.collection.mutable

/**
 * Abstract class for field in a PMML.
 */
abstract class Field extends HasDataType with HasOpType with Attribute {

  require(!isDataField || name.nonEmpty, "Cannot have an empty string for name.")

  // A flag if the field is a plain numeric field
  override lazy val isPlain: Boolean = attribute.isPlain

  /** Name of the field. */
  def name: String

  /** Display name of the field. None if it is not set. */
  def displayName: Option[String] = None

  /** Attribute of the field. */
  def attribute: Attribute = TypelessAttribute

  /** Index of the field in the input series. */
  def index: Int = -1

  /** Sets the index of this field. */
  def index_=(i: Int): Unit

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
  def toVal(s: String): DataVal = Utils.toDataVal(s, dataType)

  /**
   * Converts a string to the corresponding value based on its data type.
   *
   * @return None if any error occurs
   */
  def toValOption(s: String): Option[DataVal] = try {
    Option(Utils.toDataVal(s, dataType))
  } catch {
    case _: Exception => None
  }

  /** Retrieve its value from the specified series, return null if missing */
  def get(series: Series): DataVal

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

  def getDoubleVal(series: Series): DoubleVal = {
    val value = get(series)
    if (value != null) {
      if (value.dataType != DoubleType) {
        DataVal.from(value.toDouble)
      } else value.asInstanceOf[DoubleVal]
    } else
      DataVal.NaN
  }

  /** Converts to an immutable attribute if it's mutable. */
  def toImmutable: Field = this

  /** Tests if the field is referenced in the model element. */
  def referenced: Boolean = false

  /** Sets the referenced flag of the field. */
  def referenced_=(r: Boolean): Unit

  override def labels: Map[DataVal, String] = attribute.labels

  override def getLabel(value: DataVal): Option[String] = attribute.getLabel(value)

  override def missingValues: Set[DataVal] = attribute.missingValues

  override def isMissingValue(value: DataVal): Boolean = attribute.isMissingValue(value)

  override def invalidValues: Set[DataVal] = attribute.invalidValues

  override def isInvalidValue(value: DataVal): Boolean = attribute.isInvalidValue(value)

  override def validValues: Array[DataVal] = attribute.validValues

  override def isValidValue(value: DataVal): Boolean = attribute.isValidValue(value)

  override def intervals: Array[Interval] = attribute.intervals

  override def numCategories: Int = attribute.numCategories

  override def isBinary: Boolean = attribute.isBinary

  override def encode(value: DataVal): Double = attribute.encode(value)

  override def decode(index: Int): DataVal = attribute.decode(index)

  override def attrType: AttributeType = attribute.attrType

  override def toAttribute: Attribute = attribute.toAttribute

  override def isMutable: Boolean = attribute.isMutable

  override def isNumeric: Boolean = dataType.isNumeric

  override def isFloat: Boolean = dataType.isFloat

  override def isDouble: Boolean = dataType.isDouble

  override def isString: Boolean = dataType.isString

  override def isDate: Boolean = dataType.isDate

  override def isTime: Boolean = dataType.isTime

  override def isTimestamp: Boolean = dataType.isTimestamp
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


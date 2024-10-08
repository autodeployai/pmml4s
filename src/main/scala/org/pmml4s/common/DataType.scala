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
package org.pmml4s.common

import org.pmml4s.data.DataVal
import org.pmml4s.util.Utils
import org.pmml4s.xml.ValTags

/**
 * The base type of all PMML data types.
 */
sealed abstract class DataType extends DataTypeLike {
  def dataType: DataType = this

  def toVal(s: String): DataVal = Utils.toDataVal(s, this)
}

/**
 * Numeric data types.
 */
sealed abstract class NumericType extends DataType {
  override def isNumeric: Boolean = true
}

/**
 * The base type of date
 */
sealed abstract class DateType extends NumericType {
  override def isDate: Boolean = true

  override def toString: String = ValTags.DATE
}

/**
 * The base type of timestamp
 */
sealed abstract class DateTimeType extends NumericType {
  override def isTimestamp: Boolean = true

  override def toString: String = ValTags.DATETIME
}

/**
 * The data type representing `Time` values.
 */
class TimeType protected() extends NumericType {
  override def isTime: Boolean = true

  override def toString: String = ValTags.TIME
}

/**
 * The data type representing `String` values.
 */
class StringType private() extends DataType {
  override def isString: Boolean = true

  override def toString: String = ValTags.STRING
}

/**
 * The data type representing `Boolean` values.
 */
class BooleanType private() extends DataType {
  override def toString: String = ValTags.BOOLEAN
}

/**
 * The data type representing `Int` or `Long` values.
 */
class IntegerType private() extends NumericType {
  override def toString: String = ValTags.INTEGER
}

/**
 * The data type representing `Float` values.
 */
class FloatType private() extends NumericType {
  override def toString: String = ValTags.FLOAT

  override def isFloat: Boolean = true
}

/**
 * The data type representing `Double` values.
 */
class DoubleType private() extends NumericType {
  override def toString: String = ValTags.DOUBLE

  override def isDouble: Boolean = true
}

/**
 * The data type representing `Float` or `Double` values, the `Real` is an extended type beyond PMML
 */
class RealType private() extends NumericType {
  override def toString: String = "real"

  override def isFloat: Boolean = true

  override def isDouble: Boolean = true
}

case object UnresolvedDataType extends DataType {
  override def toString: String = "unresolved"
}

case object StringType extends StringType

case object IntegerType extends IntegerType

case object RealType extends RealType

case object FloatType extends FloatType

case object DoubleType extends DoubleType

case object BooleanType extends BooleanType

case object DateType extends DateType

case object TimeType extends TimeType

case object DateTimeType extends DateTimeType

/**
 * The type timeSeconds is a variant of the type time where the values are represented as the number of seconds since 00:00, that is,
 * since midnight. The time 00:00 is represented by the number 0. No negative values are allowed.
 */
class TimeSecondsType private() extends TimeType {
  override def toString: String = ValTags.TIMESECONDS
}

case object TimeSecondsType extends TimeSecondsType

/**
 * The type dateDaysSince[aYear] is a variant of the type date where the values are represented as the number of days since aYear-01-01.
 * The date aYear-01-01 is represented by the number 0. aYear-01-02 is represented by 1, aYear-02-01 is represented by 31, etc.
 * Dates before aYear-01-01 are represented as negative numbers.
 * For example, values of type dateDaysSince[1960] are the number of days since 1960-01-01. The date 1960-01-01 is represented by the number 0.
 */
case class DateDaySinceYearType(aYear: Int) extends DateType {
  override def toString = s"dateDaysSince${aYear}"
}

case object DateDaySinceYearType {
  val DateDaySinceYear0Type: DateDaySinceYearType = DateDaySinceYearType(0)
  val DateDaySinceYear1960Type: DateDaySinceYearType = DateDaySinceYearType(1960)
  val DateDaySinceYear1970Type: DateDaySinceYearType = DateDaySinceYearType(1970)
  val DateDaySinceYear1980Type: DateDaySinceYearType = DateDaySinceYearType(1980)
}

/**
 * The type dateTimeSecondsSince[aYear] is a variant of the type date where the values are represented as the number of seconds since 00:00 on aYear-01-01.
 * The datetime 00:00:00 on aYear-01-01 is represented by the number 0. The datetime 00:00:01 on aYear-01-01 is represented by 1, etc.
 * Datetimes before aYear-01-01 are represented as negative numbers.
 * For example, values of type dateTimeSecondsSince[1960] are the number of seconds since 00:00 on 1960-01-01. The datetime 00:00:00 on 1960-01-01 is represented by the number 0.
 * The datetime 00:01:00 on 1960-01-01 is represented by 60.
 */
case class DateTimeSecondSinceYearType(aYear: Int) extends DateTimeType {
  override def toString = s"dateTimeSecondsSince${aYear}"
}

case object DateTimeSecondSinceYearType {
  val DateTimeSecondSinceYear0Type: DateTimeSecondSinceYearType = DateTimeSecondSinceYearType(0)
  val DateTimeSecondSinceYear1960Type: DateTimeSecondSinceYearType = DateTimeSecondSinceYearType(1960)
  val DateTimeSecondSinceYear1970Type: DateTimeSecondSinceYearType = DateTimeSecondSinceYearType(1970)
  val DateTimeSecondSinceYear1980Type: DateTimeSecondSinceYearType = DateTimeSecondSinceYearType(1980)
}

/**
 * A field inside a StructType.
 *
 * @param name     The name of this field.
 * @param dataType The data type of this field.
 */
case class StructField(name: String, dataType: DataType)

/**
 * StructType defines a type for a [Series]
 */
case class StructType(fields: Array[StructField]) extends DataType with Seq[StructField] {
  def this() = this(Array.empty[StructField])

  /** Returns all field names in an array. */
  def fieldNames: Array[String] = fields.map(_.name)

  private lazy val nameToField = Utils.toMap(fields.map(_.name), fields)
  private lazy val nameToIndex = Utils.toMapWithIndex(fieldNames)

  def add(field: StructField): StructType = {
    StructType(fields :+ field)
  }

  def add(name: String, dataType: DataType): StructType = {
    StructType(fields :+ StructField(name, dataType))
  }

  /**
   * Extracts the [[StructField]] with the given name.
   *
   * @throws IllegalArgumentException if a field with the given name does not exist
   */
  def apply(name: String): StructField = {
    nameToField.getOrElse(name,
      throw new IllegalArgumentException(s"""Field "$name" does not exist."""))
  }

  /**
   * Returns the index of a given field.
   *
   * @throws IllegalArgumentException if a field with the given name does not exist
   */
  def fieldIndex(name: String): Int = {
    nameToIndex.getOrElse(name,
      throw new IllegalArgumentException(s"""Field "$name" does not exist."""))
  }

  def getFieldIndex(name: String): Option[Int] = {
    nameToIndex.get(name)
  }

  def fieldName(fieldIndex: Int): String = fields(fieldIndex).name

  override def apply(fieldIndex: Int): StructField = fields(fieldIndex)

  override def length: Int = fields.length

  override def iterator: Iterator[StructField] = fields.iterator
}

object StructType {
  def apply(fields: Seq[StructField]): StructType = StructType(fields.toArray)
}

object DataType {

  import DateDaySinceYearType._
  import DateTimeSecondSinceYearType._

  val string = StringType
  val float = FloatType
  val double = DoubleType
  val integer = IntegerType
  val boolean = BooleanType
  val date = DateType
  val time = TimeType
  val dateTime = DateTimeType
  val `dateDaysSince[0]` = DateDaySinceYear0Type
  val `dateDaysSince[1960]` = DateDaySinceYear1960Type
  val `dateDaysSince[1970]` = DateDaySinceYear1970Type
  val `dateDaysSince[1980]` = DateDaySinceYear1980Type
  val timeSeconds = TimeSecondsType
  val `dateTimeSecondsSince[0]` = DateTimeSecondSinceYear0Type
  val `dateTimeSecondsSince[1960]` = DateTimeSecondSinceYear1960Type
  val `dateTimeSecondsSince[1970]` = DateTimeSecondSinceYear1970Type
  val `dateTimeSecondsSince[1980]` = DateTimeSecondSinceYear1980Type

  /** Extended type */
  val real = RealType
  val REAL = RealType

  /** Defines const variables could be used in Java, names above could be invalid for Java. */
  val STRING = StringType
  val FLOAT = FloatType
  val DOUBLE = DoubleType
  val INTEGER = IntegerType
  val BOOLEAN = BooleanType
  val DATE = DateType
  val TIME = TimeType
  val DATETIME = DateTimeType
  val DATE_DAYS_SINCE_0 = DateDaySinceYear0Type
  val DATE_DAYS_SINCE_1960 = DateDaySinceYear1960Type
  val DATE_DAYS_SINCE_1970 = DateDaySinceYear1970Type
  val DATE_DAYS_SINCE_1980 = DateDaySinceYear1980Type
  val TIME_SECONDS = TimeSecondsType
  val DATE_TIME_SECONDS_SINCE_0 = DateTimeSecondSinceYear0Type
  val DATE_TIME_SECONDS_SINCE_1960 = DateTimeSecondSinceYear1960Type
  val DATE_TIME_SECONDS_SINCE_1970 = DateTimeSecondSinceYear1970Type
  val DATE_TIME_SECONDS_SINCE_1980 = DateTimeSecondSinceYear1980Type

  def withName(s: String): DataType =
    s match {
      case ValTags.STRING                       => StringType
      case ValTags.INTEGER                      => IntegerType
      case ValTags.FLOAT                        => FloatType
      case ValTags.DOUBLE                       => DoubleType
      case ValTags.BOOLEAN                      => BooleanType
      case ValTags.DATE                         => DateType
      case ValTags.TIME                         => TimeType
      case ValTags.DATETIME                     => DateTimeType
      case ValTags.`DATEDAYSSINCE[0]`           => DateDaySinceYear0Type
      case ValTags.`DATEDAYSSINCE[1960]`        => DateDaySinceYear1960Type
      case ValTags.`DATEDAYSSINCE[1970]`        => DateDaySinceYear1970Type
      case ValTags.`DATEDAYSSINCE[1980]`        => DateDaySinceYear1980Type
      case ValTags.TIMESECONDS                  => TimeSecondsType
      case ValTags.`DATETIMESECONDSSINCE[0]`    => DateTimeSecondSinceYear0Type
      case ValTags.`DATETIMESECONDSSINCE[1960]` => DateTimeSecondSinceYear1960Type
      case ValTags.`DATETIMESECONDSSINCE[1970]` => DateTimeSecondSinceYear1970Type
      case ValTags.`DATETIMESECONDSSINCE[1980]` => DateTimeSecondSinceYear1980Type
      case _                                    => UnresolvedDataType
    }
}

/**
 * A template trait for a data type.
 */
trait DataTypeLike extends Serializable {

  /** Returns the data type of field. */
  def dataType: DataType

  /** Returns true if `other` is an acceptable input type for a function that expects this. */
  def accepts(other: DataType): Boolean = this == other

  def isNumeric: Boolean = false

  def isFloat: Boolean = false

  def isDouble: Boolean = false

  def isString: Boolean = false

  def isDate: Boolean = false

  def isTime: Boolean = false

  def isTimestamp: Boolean = false

  def isDateTime: Boolean = isDate || isTime || isTimestamp

  def isReal: Boolean = isFloat || isDouble
}

trait HasDataType extends DataTypeLike


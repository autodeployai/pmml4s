/*
 * Copyright (c) 2024 AutoDeployAI
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
package org.pmml4s.data

import org.pmml4s.common._
import org.pmml4s.data.DataVal.DefaultDecimalFormat
import org.pmml4s.util.StringUtils

import java.text.DecimalFormat
import java.util.Date
import scala.reflect.ClassTag

sealed trait DataVal extends Serializable {
  def isMissing: Boolean = false

  def nonMissing: Boolean = true

  def isNumeric: Boolean

  def isString: Boolean

  def dataType: DataType = UnresolvedDataType

  def toVal: Any

  def toDouble: Double

  def toLong: Long

  def toInt: Int

  def toByte: Byte

  def toShort: Short

  def toChar: Char

  def toBool: Boolean

  @inline def as[T: ClassTag]: T = {
    val tag = implicitly[ClassTag[T]]
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => toByte.asInstanceOf[T]
      case java.lang.Short.TYPE     => toShort.asInstanceOf[T]
      case java.lang.Character.TYPE => toChar.asInstanceOf[T]
      case java.lang.Integer.TYPE   => toInt.asInstanceOf[T]
      case java.lang.Long.TYPE      => toLong.asInstanceOf[T]
      case java.lang.Float.TYPE     => toInt.asInstanceOf[T]
      case java.lang.Double.TYPE    => toInt.asInstanceOf[T]
      case java.lang.Boolean.TYPE   => toInt.asInstanceOf[T]
      case java.lang.Void.TYPE      => toInt.asInstanceOf[T]
      case _                        => toVal.asInstanceOf[T]
    }
  }
}

object DataVal {
  val TYPE: Class[DataVal] = classOf[DataVal]
  val DefaultDecimalFormat = new DecimalFormat()

  val `1.0`: DoubleVal = DoubleVal(1.0)
  val `0.0`: DoubleVal = DoubleVal(0.0)
  val `1`: LongVal = LongVal(1L)
  val NaN: DoubleVal = DoubleVal(java.lang.Double.NaN)
  val PositiveInfinity: DoubleVal = DoubleVal(java.lang.Double.POSITIVE_INFINITY)
  val NegativeInfinity: DoubleVal = DoubleVal(java.lang.Double.NEGATIVE_INFINITY)
  val NULLString: StringVal = StringVal(null)
  val EmptyString: StringVal = StringVal("")
  val NULLDate: DateVal = DateVal(null)
  val NULL: DataVal = NullVal
  val TRUE: BoolVal = BoolVal(true)
  val FALSE: BoolVal = BoolVal(false)

  def from(value: Double): DoubleVal = if (value != value) NaN else DoubleVal(value)
  def from(value: Float): FloatVal = FloatVal(value)
  def from(value: BigDecimal): DoubleVal = DoubleVal(value.doubleValue)
  def from(value: String): StringVal = if (value == null) NULLString else StringVal(value)
  def from(value: Int): LongVal = LongVal(value)
  def from(value: Long): LongVal = LongVal(value)
  def from(value: Boolean): BoolVal = if (value) TRUE else FALSE
  def from(value: Date): DateVal = if (value == null) NULLDate else DateVal(value)
  def from(values: Array[Any]): Array[DataVal] = values.map(from)
  def from(values: Set[Any]): Set[DataVal] = values.map(from)
  def from(values: Map[String, Any]): Map[String, DataVal] = values.map(entry => (entry._1, from(entry._2)))

  def from(value: Any): DataVal = value match {
    case x: DataVal     => x
    case x: Double      => from(x)
    case x: Int         => from(x)
    case x: Long        => from(x)
    case x: String      => from(x)
    case x: Boolean     => from(x)
    case x: Float       => from(x)
    case x: Short       => from(x)
    case x: Byte        => from(x)
    case x: BigDecimal  => from(x)
    case x: Date        => from(x)
    case _              => NULL
  }

  def apply(value: Any): DataVal = if (value == null) NULL else from(value)
}

trait NumericVal extends DataVal {
  override def isNumeric: Boolean = true

  override def isString: Boolean = false
}

case class DoubleVal(value: Double) extends NumericVal {
  @inline
  override def isMissing: Boolean = value != value

  @inline
  override def nonMissing: Boolean = value == value

  override def dataType: DataType = DoubleType

  override def toVal: Any = value

  override def toDouble: Double = value

  override def toLong: Long = value.toLong

  override def toInt: Int = value.toInt

  override def toByte: Byte = value.toByte

  override def toShort: Short = value.toShort

  override def toChar: Char = value.toChar

  override def toBool: Boolean = value != 0.0

  override def toString: String = {
    DefaultDecimalFormat.format(value)
  }

  def <(x: Double): Boolean = value < x

  def <(x: DoubleVal): Boolean = value < x.value

  def <=(x: Double): Boolean = value <= x

  def <=(x: DoubleVal): Boolean = value <= x.value

  def >(x: Double): Boolean = value > x

  def >(x: DoubleVal): Boolean = value > x.value

  def >=(x: Double): Boolean = value >= x

  def >=(x: DoubleVal): Boolean = value >= x.value

  def equals(that: Double): Boolean = value == that
}

case class FloatVal(value: Float) extends NumericVal {
  override def dataType: DataType = FloatType

  override def toVal: Any = value

  override def toDouble: Double = value.toDouble

  override def toLong: Long = value.toLong

  override def toInt: Int = value.toInt

  override def toByte: Byte = value.toByte

  override def toShort: Short = value.toShort

  override def toChar: Char = value.toChar

  override def toBool: Boolean = value != 0.0f

  override def toString: String = java.lang.Float.toString(value)

  def <(x: Float): Boolean = value < x

  def <(x: FloatVal): Boolean = value < x.value

  def <=(x: Float): Boolean = value <= x

  def <=(x: FloatVal): Boolean = value <= x.value

  def >(x: Float): Boolean = value > x

  def >(x: FloatVal): Boolean = value > x.value

  def >=(x: Float): Boolean = value >= x

  def >=(x: FloatVal): Boolean = value >= x.value

  def equals(that: Float): Boolean = value == that
}

case class LongVal(value: Long) extends NumericVal {
  override def dataType: DataType = IntegerType

  override def toVal: Any = value

  override def toDouble: Double = value.toDouble

  override def toLong: Long = value

  override def toInt: Int = value.toInt

  override def toByte: Byte = value.toByte

  override def toShort: Short = value.toShort

  override def toChar: Char = value.toChar

  override def toBool: Boolean = value != 0L

  override def toString: String = java.lang.Long.toString(value)

  def <(x: Long): Boolean = value < x
  def <(x: Int): Boolean = value < x
  def <(x: LongVal): Boolean = value < x.value

  def <=(x: Long): Boolean = value <= x
  def <=(x: Int): Boolean = value <= x
  def <=(x: LongVal): Boolean = value <= x.value

  def >(x: Long): Boolean = value > x
  def >(x: Int): Boolean = value > x
  def >(x: LongVal): Boolean = value > x.value

  def >=(x: Long): Boolean = value >= x
  def >=(x: Int): Boolean = value >= x
  def >=(x: LongVal): Boolean = value >= x.value

  def equals(that: Long): Boolean = value == that
  def equals(that: Int): Boolean = value == that
}

case class BoolVal(value: Boolean) extends DataVal {
  override def isNumeric: Boolean = true

  override def isString: Boolean = true

  override def dataType: DataType = BooleanType

  override def toVal: Any = value

  override def toDouble: Double = if (value) 1.0 else 0.0

  override def toLong: Long = if (value) 1L else 0L

  override def toInt: Int = if (value) 1 else 0

  def toByte: Byte = if (value) 1 else 0

  def toShort: Short = if (value) 1 else 0

  def toChar: Char = if (value) 1 else 0

  override def toBool: Boolean = value

  override def toString: String = java.lang.Boolean.toString(value)
}

case class StringVal(value: String) extends DataVal {
  @inline
  override def isMissing: Boolean = value == null

  @inline
  override def nonMissing: Boolean = value != null

  override def isNumeric: Boolean = false

  override def isString: Boolean = true

  override def dataType: DataType = StringType

  override def toVal: Any = value

  override def toDouble: Double = StringUtils.asDouble(value)

  override def toLong: Long = java.lang.Long.parseLong(value)

  override def toInt: Int = java.lang.Integer.parseInt(value)

  override def toByte: Byte = java.lang.Byte.parseByte(value)

  override def toShort: Short = java.lang.Short.parseShort(value)

  override def toChar: Char = if (value != null && value.nonEmpty) value.head else Char.MinValue

  override def toBool: Boolean = value.toBoolean

  override def toString: String = value
}

case class DateVal(value: Date) extends NumericVal {

  override def isMissing: Boolean = value == null

  override def dataType: DataType = DateType

  override def isNumeric: Boolean = true

  override def isString: Boolean = true

  override def toVal: Any = value

  override def toDouble: Double = value.getTime.toDouble

  override def toLong: Long = value.getTime

  override def toInt: Int = value.getTime.toInt

  override def toByte: Byte = value.getTime.toByte

  override def toShort: Short = value.getTime.toShort

  override def toChar: Char = value.getTime.toChar

  override def toBool: Boolean = false

  override def toString: String = value.toString

  def toDate: Date = value
}

object NullVal extends DataVal {
  @inline
  override def isMissing: Boolean = true

  override def nonMissing: Boolean = false

  override def isNumeric: Boolean = true

  override def isString: Boolean = true

  override def toVal: Any = null

  override def toDouble: Double = Double.NaN

  override def toLong: Long = Long.MinValue

  override def toInt: Int = Int.MinValue

  override def toByte: Byte = Byte.MinValue

  override def toShort: Short = Short.MinValue

  override def toChar: Char = Char.MinValue

  override def toBool: Boolean = false

  override def toString: String = ""
}
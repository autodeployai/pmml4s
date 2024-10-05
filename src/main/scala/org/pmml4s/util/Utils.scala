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
package org.pmml4s.util

import org.pmml4s.common._
import org.pmml4s.data.{BoolVal, DataVal, DoubleVal, FloatVal, LongVal, NullVal, NumericVal, StringVal}

import scala.annotation.tailrec
import scala.collection.mutable.{AnyRefMap, Iterable}
import scala.collection.mutable

/**
 * Various utility methods used by PMML4S.
 */
object Utils {

  def isNumeric(value: Any): Boolean = value match {
    case _: NumericVal | _: Double | _: Float | _: Int | _: Long | _: Short | _: Byte => true
    case _                                                            => false
  }

  def toDouble(value: Any): Double = value match {
    case x: DataVal    => x.toDouble
    case x: Double     => x
    case x: Float      => x.toDouble
    case x: BigDecimal => x.doubleValue
    case x: Int        => x.toDouble
    case x: Long       => x.toDouble
    case x: Short      => x.toDouble
    case x: Byte       => x.toDouble
    case x: String     => StringUtils.toDouble(x, Double.NaN)
    case _             => Double.NaN
  }

  @inline
  def toString(a: DataVal): String = if (a != null) a.toString else null

  def toString(a: Any): String = a match {
    case null => null
    case _    => a.toString
  }

  def toLong(a: Any): Long = a match {
    case x: DataVal    => x.toLong
    case x: Long       => x
    case x: Int        => x.toLong
    case x: BigDecimal => x.longValue
    case x: Double     => x.toLong
    case x: Float      => x.toLong
    case x: Short      => x.toLong
    case x: Byte       => x.toLong
    case x: String     => StringUtils.toLong(x, Long.MinValue)
    case _             => Long.MinValue
  }

  def toInt(a: Any): Int = a match {
    case x: DataVal    => x.toInt
    case x: Int        => x
    case x: Long       => x.toInt
    case x: BigDecimal => x.intValue
    case x: Double     => x.toInt
    case x: Float      => x.toInt
    case x: Short      => x.toInt
    case x: Byte       => x.toInt
    case x: String     => StringUtils.toInt(x, Int.MinValue)
    case _             => Int.MinValue
  }

  def toBoolean(a: Any): Boolean = a match {
    case x: DataVal => x.toBool
    case x: Boolean => x
    case x: Int     => x != 0
    case x: Long    => x != 0L
    case x: Short   => x != 0
    case x: Byte    => x != 0
    case x: Double  => x != 0.0
    case x: Float   => x != 0.0f
    case x: String  => x.toBoolean
    case _          => false
  }

  def inferDataType(a: Any): DataType = a match {
    case _: DoubleVal | _: FloatVal | _: Double | _: Float | _:BigDecimal   => RealType
    case _: LongVal | _: Long | _: Int | _: Short | _: Byte                 => IntegerType
    case _: String                                                          => StringType
    case _: StringVal                                                       => StringType
    case _: BoolVal | _: Boolean                                            => BooleanType
    case _                                                                  => UnresolvedDataType
  }

  def toVal(s: String, dataType: DataType): Any = dataType match {
    case IntegerType    => {
      try {
        s.toLong
      } catch {
        // Support such float number, for example "1.0", which is converted into double firstly,
        // then converted to integer again.
        case _: NumberFormatException => {
          val d = StringUtils.asDouble(s)
          if (d != d) {
            null
          } else {
            d.toLong
          }
        }
        case e: Throwable             => null
      }
    }
    case _: NumericType => StringUtils.asDouble(s)
    case BooleanType    => s.toBoolean
    case _              => s
  }

  def toDataVal(s: String, dataType: DataType): DataVal = dataType match {
    case IntegerType    => {
      try {
        LongVal(s.toLong)
      } catch {
        // Support such float number, for example "1.0", which is converted into double firstly,
        // then converted to integer again.
        case _: NumberFormatException => {
          val d = StringUtils.asDouble(s)
          if (d != d) {
            DataVal.NULL
          } else {
            LongVal(d.toLong)
          }
        }
        case e: Throwable             => DataVal.NULL
      }
    }
    case _: NumericType => DataVal.from(StringUtils.asDouble(s))
    case BooleanType    => BoolVal(s.toBoolean)
    case _              => StringVal(s)
  }

  def getVal(s: String, dataType: DataType): Option[Any] = dataType match {
    case IntegerType    => {
      try {
        Option(s.toLong)
      } catch {
        // Support such float number, for example "1.0", which is converted into double firstly,
        // then converted to integer again.
        case _: NumberFormatException => {
          StringUtils.toDouble(s).map(_.toLong)
        }
        case e: Throwable             => None
      }
    }
    case _: NumericType => StringUtils.toDouble(s)
    case BooleanType    => StringUtils.toBool(s)
    case _              => Option(s)
  }

  def getDataVal(s: String, dataType: DataType): Option[DataVal] = dataType match {
    case IntegerType    => {
      try {
        Option(LongVal(s.toLong))
      } catch {
        // Support such float number, for example "1.0", which is converted into double firstly,
        // then converted to integer again.
        case _: NumberFormatException => {
          StringUtils.toDouble(s).map(x => LongVal(x.toLong))
        }
        case e: Throwable             => None
      }
    }
    case _: NumericType => StringUtils.toDouble(s).map(DoubleVal.apply)
    case BooleanType    => StringUtils.toBool(s).map(BoolVal.apply)
    case _              => Option(s).map(StringVal.apply)
  }

  def toVal(a: Any, dataType: DataType): Any = dataType match {
    case IntegerType        => toLong(a)
    case _: NumericType     => toDouble(a)
    case BooleanType        => toBoolean(a)
    case UnresolvedDataType => a
    case _                  => toString(a)
  }

  @tailrec
  def toDataVal(a: Any, dataType: DataType): DataVal = dataType match {
    case x: DataVal         => x
    case IntegerType        => LongVal(toLong(a))
    case _: NumericType     => DoubleVal(toDouble(a))
    case BooleanType        => BoolVal(toBoolean(a))
    case UnresolvedDataType => {
      val dataTypeInferred = inferDataType(a)
      if (dataTypeInferred != UnresolvedDataType) toDataVal(a, dataTypeInferred) else NullVal
    }
    case _                  => StringVal(toString(a))
  }

  @inline
  def isNull(value: DataVal): Boolean = (value == null || value == DataVal.NULL)

  @inline
  def isMissing(value: DataVal): Boolean = (value == null || value.isMissing)

  def isMissing(value: Any): Boolean = (value == null) || (value match {
    case x: DataVal => x.isMissing
    case x: Double => x != x
    case _         => false
  })

  def orNull(a: Any, b: => Any): Any = {
    if (a == null) null else b
  }

  @inline
  def nonMissing(value: DataVal): Boolean = (value != null && value.nonMissing)

  def nonMissing(value: Any): Boolean = !isMissing(value)

  @inline
  def isMissing(value: Double): Boolean = value != value

  def nonMissing(value: Double): Boolean = value == value

  def anyMissing(values: Array[Double]): Boolean = values.exists(x => x != x)

  def reduceByKey[K, V](collection: Iterable[(K, V)])(implicit num: Numeric[V]): Map[K, V] = {
    import num._
    collection.groupBy(_._1).map { case (_, traversable) => traversable.reduce((a, b) => (a._1, a._2 + b._2)) }
  }

  def plus[T](lhs: Seq[T], rhs: Seq[Double])(implicit num: Numeric[T]): Seq[Double] = {
    for (i <- 0 until Math.min(lhs.size, rhs.size)) yield num.toDouble(lhs(i)) + rhs(i)
  }

  def toMapWithIndex[K <: AnyRef](keys: Array[K]): mutable.Map[K, Int] = {
    val sz = keys.length
    val arm = new AnyRefMap[K, Int](sz)
    var i = 0
    while (i < sz) { arm(keys(i)) = i; i += 1 }
    arm.repack()
    arm
  }

  def toMap[K <: AnyRef, V](keys: Array[K], values: Array[V]): mutable.Map[K, V] = {
    val sz = math.min(keys.length, values.length)
    val arm = new AnyRefMap[K, V](sz)
    var i = 0
    while (i < sz) { arm(keys(i)) = values(i); i += 1 }
    arm.repack()
    arm
  }

  def toMap[K <: AnyRef, V](keys: Iterable[K], values: Iterable[V]): mutable.Map[K, V] = {
    val sz = math.min(keys.size, values.size)
    val arm = new AnyRefMap[K, V](sz)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) arm(ki.next()) = vi.next
    arm.repack()
    arm
  }
}


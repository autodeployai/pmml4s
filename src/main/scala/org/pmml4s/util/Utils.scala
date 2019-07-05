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
package org.pmml4s.util

import org.pmml4s.common._

/**
 * Various utility methods used by PMML4S.
 */
object Utils {

  def isNumeric(value: Any): Boolean = value match {
    case _: Double | _: Float | _: Int | _: Long | _: Short | _: Byte => true
    case _                                                            => false
  }

  def toDouble(value: Any): Double = value match {
    case x: Double     => x
    case x: Float      => x.toDouble
    case x: BigDecimal => x.doubleValue()
    case x: Int        => x.toDouble
    case x: Long       => x.toDouble
    case x: Short      => x.toDouble
    case x: Byte       => x.toDouble
    case x: String     => StringUtils.toDouble(x, Double.NaN)
    case _             => Double.NaN
  }

  def toString(a: Any): String = a match {
    case null => null
    case _    => a.toString
  }

  def toLong(a: Any): Long = a match {
    case x: Long       => x
    case x: Int        => x.toLong
    case x: BigDecimal => x.longValue()
    case x: Double     => x.toLong
    case x: Float      => x.toLong
    case x: Short      => x.toLong
    case x: Byte       => x.toLong
    case x: String     => StringUtils.toLong(x, Long.MinValue)
    case _             => Long.MinValue
  }

  def toInt(a: Any): Int = a match {
    case x: Int        => x
    case x: Long       => x.toInt
    case x: BigDecimal => x.intValue()
    case x: Double     => x.toInt
    case x: Float      => x.toInt
    case x: Short      => x.toInt
    case x: Byte       => x.toInt
    case x: String     => StringUtils.toInt(x, Int.MinValue)
    case _             => Int.MinValue
  }

  def toBoolean(a: Any): Boolean = a match {
    case x: Boolean => x
    case x: Int     => x != 0
    case x: Long    => x != 0L
    case x: Short   => x != 0
    case x: Byte    => x != 0
    case x: Double  => x != 0.0
    case x: Float   => x != 0.0f
    case x: String  => x.compareToIgnoreCase("true") == 0
    case _          => false
  }

  def inferDataType(a: Any): DataType = a match {
    case _: Double | Float | BigDecimal => RealType
    case _: Long | Int | Short | Byte   => IntegerType
    case _: String                      => StringType
    case _: Boolean                     => BooleanType
    case _                              => UnresolvedDataType
  }

  def toVal(s: String, dataType: DataType): Any = dataType match {
    case IntegerType    => {
      try {
        s.toLong
      } catch {
        // Support such float number, for example "1.0", which is converted into double firstly,
        // then converted to integer again.
        case _: NumberFormatException => {
          s.toDouble.toLong
        }
        case e: Throwable             => throw e
      }
    }
    case _: NumericType => s.toDouble
    case BooleanType    => s.toBoolean
    case _              => s
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

  def toVal(a: Any, dataType: DataType): Any = dataType match {
    case UnresolvedDataType => a
    case IntegerType        => toLong(a)
    case _: NumericType     => toDouble(a)
    case BooleanType        => toBoolean(a)
    case _                  => toString(a)
  }

  def isMissing(value: Any): Boolean = (value == null) || (value match {
    case x: Double => x != x
    case _         => false
  })

  def orNull(a: Any, b: => Any): Any = {
    if (a == null) null else b
  }

  def nonMissing(value: Any): Boolean = !isMissing(value)

  def isMissing(value: Double): Boolean = value != value

  def nonMissing(value: Double): Boolean = value == value

  def reduceByKey[K, V](collection: Traversable[(K, V)])(implicit num: Numeric[V]): Map[K, V] = {
    import num._
    collection.groupBy(_._1).map { case (_: K, traversable) => traversable.reduce((a, b) => (a._1, a._2 + b._2)) }
  }

  def plus[T](lhs: Seq[T], rhs: Seq[Double])(implicit num: Numeric[T]): Seq[Double] = {
    for (i <- 0 until Math.min(lhs.size, rhs.size)) yield num.toDouble(lhs(i)) + rhs(i)
  }
}
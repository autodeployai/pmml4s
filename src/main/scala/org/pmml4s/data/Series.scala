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
package org.pmml4s.data

import java.util

import org.pmml4s.common._
import org.pmml4s.util.JsUtils._
import org.pmml4s.util.Utils
import spray.json._

import scala.collection.mutable

object Series {

  /**
   * This method can be used to extract fields from a [[Series]] object in a pattern match. Example:
   * {{{
   * import org.pmml4s.data._
   *
   * val pairs = data.map {
   *   case Series(key: Int, value: String) =>
   *     key -> value
   * }
   * }}}
   */
  def unapplySeq(row: Series): Some[Seq[Any]] = Some(row.toSeq)

  /**
   * This method can be used to construct a [[Series]] with the given values.
   */
  def apply(values: Any*): Series = new GenericSeries(values.toArray)

  /**
   * This method can be used to construct a [[Series]] from a [[Seq]] of values.
   */
  def fromSeq(values: Seq[Any]): Series = new GenericSeries(values.toArray)
  
  def fromArray(values: Array[Any]): Series = new GenericSeries(values)  

  def fromTuple(tuple: Product): Series = fromSeq(tuple.productIterator.toSeq)

  def fromSeq(values: Seq[Any], schema: StructType): Series = new GenericSeriesWithSchema(values.toArray, schema)

  def fromArray(values: Array[Any], schema: StructType): Series = new GenericSeriesWithSchema(values, schema)
  
  def fromTuple(tuple: Product, schema: StructType): Series = fromSeq(tuple.productIterator.toSeq, schema)

  def fromSplit(columns: Seq[String], values: Seq[Any]): Series = {
    require(values.size == columns.size, s"Should be same length, got columns=${columns.size} and values=${values.size}.")

    val fields = columns.map(x => StructField(x, UnresolvedDataType)).toArray
    new GenericSeriesWithSchema(values.toArray, StructType(fields))
  }  
  
  def fromMap(map: Map[String, Any]): Series = {
    val values = new Array[Any](map.size)
    val fields = new Array[StructField](map.size)

    var i = 0
    for (pair <- map) {
      fields(i) = StructField(pair._1, UnresolvedDataType)
      values(i) = pair._2
      i += 1
    }

    new GenericSeriesWithSchema(values, StructType(fields))
  }

  def fromMap(map: java.util.Map[String, Any]): Series = {
    val values = new Array[Any](map.size)
    val fields = new Array[StructField](map.size)

    var i = 0
    val it = map.entrySet().iterator()
    while (it.hasNext) {
      val pair = it.next()
      fields(i) = StructField(pair.getKey, UnresolvedDataType)
      values(i) = pair.getValue
      i += 1
    }

    new GenericSeriesWithSchema(values, StructType(fields))
  }

  def fromMap(map: Map[String, Any], schema: StructType): Series = {
    val values = schema.fields.map(x => {
      map.get(x.name).map(y => Utils.toVal(y, x.dataType)).orNull
    })
    new GenericSeriesWithSchema(values, schema)
  }

  def fromMap(map: java.util.Map[String, Any], schema: StructType): Series = {
    val values = new Array[Any](schema.size)
    var i = 0
    val len = schema.size
    while (i < len) {
      val f = schema(i)
      val v = map.get(f.name)
      values(i) = if (v != null) Utils.toVal(v, f.dataType) else null
      i += 1
    }

    new GenericSeriesWithSchema(values, schema)
  }

  def fromMap(map: JsObject, schema: StructType): Series = {
    val values = schema.fields.map(x => {
      map.fields.get(x.name).map(y => Utils.toVal(y.convertTo, x.dataType)).orNull
    })

    new GenericSeriesWithSchema(values, schema)
  }

  /**
   * Merge multiple rows into a single series, one after another.
   */
  def merge(rows: Series*): Series = {
    new CombinedSeries(rows.toArray)
  }

  def merge(rows: Array[Series]): Series = {
    new CombinedSeries(rows)
  }

  /** Returns an empty row. */
  val empty = apply()
}

/**
 * Represents one series of output from a relational operator.  Allows both generic access by ordinal,
 * which will incur boxing overhead for primitives, as well as native primitive access.
 *
 * It is invalid to use the native primitive interface to retrieve a value that is null, instead a
 * user must check `isNullAt` before attempting to retrieve a value that might be null.
 */
trait Series {

  /** Number of elements in the Series. */
  def size: Int = length

  /** Number of elements in the Series. */
  def length: Int

  /**
   * Schema for the Series.
   */
  def schema: StructType = null

  /**
   * Returns the value at position i. If the value is null, null is returned.
   */
  def apply(i: Int): Any = get(i)

  /**
   * Returns the value at position i. If the value is null, null is returned.
   */
  def get(i: Int): Any

  /** Checks whether the value at position i is null. */
  def isNullAt(i: Int): Boolean = get(i) == null

  /**
   * Checks whether the value at position i is missing (null or Double.NaN) if the position is valid,
   * otherwise treated as missing too.
   */
  def isMissingAt(i: Int): Boolean = i < 0 || Utils.isMissing(get(i))

  def getDouble(i: Int): Double = Utils.toDouble(get(i))

  def getString(i: Int): String = Utils.toString(get(i))

  def getLong(i: Int): Long = Utils.toLong(get(i))

  def getInt(i: Int): Int = Utils.toInt(get(i))

  def getBoolean(i: Int): Boolean = Utils.toBoolean(get(i))

  def toSeq: Seq[Any] = {
    toArray.toSeq
  }

  def toArray: Array[Any] = {
    val n = length
    val values = new Array[Any](n)
    var i = 0
    while (i < n) {
      values.update(i, get(i))
      i += 1
    }
    values
  }

  def toMap: Map[String, Any] = {
    val result = new mutable.HashMap[String, Any]
    val len = size
    result.sizeHint(len)
    var i = 0
    while (i < len) {
      result.put(if (schema != null) schema.fieldName(i) else i.toString, get(i))
      i += 1
    }

    result.toMap
  }


  def toJavaMap: java.util.Map[String, Any] = {
    val len = size
    val result = new util.HashMap[String, Any](len)
    var i = 0
    while (i < len) {
      result.put(if (schema != null) schema.fieldName(i) else i.toString, get(i))
      i += 1
    }

    result
  }

  def toPairSeq: Seq[(String, Any)] = {
    if (schema != null) {
      schema.fieldNames.zip(toArray).toSeq
    } else {
      0 until size map (_.toString) zip toArray
    }
  }

  /**
   * Returns the value at position i.
   * For primitive types if value is null it returns 'zero value' specific for primitive
   * ie. 0 for Int - use isNullAt to ensure that value is not null
   *
   * @throws ClassCastException when data type does not match.
   */
  def getAs[T](i: Int): T = get(i).asInstanceOf[T]

  /**
   * Returns the value of a given fieldName.
   * For primitive types if value is null it returns 'zero value' specific for primitive
   * ie. 0 for Int - use isNullAt to ensure that value is not null
   *
   * @throws UnsupportedOperationException when schema is not defined.
   * @throws IllegalArgumentException      when fieldName do not exist.
   * @throws ClassCastException            when data type does not match.
   */
  def getAs[T](fieldName: String): T = getAs[T](fieldIndex(fieldName))

  /**
   * Returns the index of a given field name.
   */
  def fieldIndex(name: String): Int = -1

  def mkString(sep: String): String = toSeq.mkString(sep)

  override def toString: String = if (schema != null) s"[${this.mkString(",")}],[${schema.map(x => s"(${x.name},${x.dataType})").mkString(",")}]" else s"[${this.mkString(",")}]"

  /**
   * Make a copy of the current [[Series]] object.
   */
  def copy(): Series

  /** Returns true if there are any NULL values in this series. */
  def anyNull: Boolean = {
    val len = length
    var i = 0
    while (i < len) {
      if (isNullAt(i)) {
        return true
      }
      i += 1
    }
    false
  }

  def anyMissing: Boolean = {
    val len = length
    var i = 0
    while (i < len) {
      if (isMissingAt(i)) {
        return true
      }
      i += 1
    }
    false
  }

  def show(): Unit = {
    println(toString)
  }

  def toJson(isObj: Boolean = true): JsValue = if (isObj) {
    if (schema == null) {
      throw new IllegalArgumentException("Schema is not defined")
    }
    JsObject(toMap.map(x => (x._1, x._2.toJson)))
  } else {
    JsArray(toArray.map(x => x.toJson).toVector)
  }

  def columns: Array[String] = {
    if (schema == null) {
      throw new IllegalArgumentException("Schema is not defined")
    }
    schema.fieldNames
  }

  def filter(filter: Seq[String]): Series = {
    if (filter == null || filter.isEmpty) {
      this
    } else {
      if (schema == null) {
        Series.empty
      } else {
        val idxBuilder = mutable.ArrayBuilder.make[Int]
        idxBuilder.sizeHint(filter.size)
        for (name <- filter) {
          schema.getFieldIndex(name).foreach(x => idxBuilder+= x)
        }

        val idx = idxBuilder.result()
        new GenericSeriesWithSchema(idx.map(get), StructType(idx.map(schema.apply)))
      }
    }
  }
}

/**
 * A series implementation that uses an array of objects as the underlying storage.
 */
class GenericSeries(val values: Array[Any]) extends Series {

  def this() = this(Array.empty[Any])

  def this(size: Int) = this(new Array[Any](size))

  override def length: Int = values.length

  override def get(i: Int): Any = if (i < 0 || i >= values.length) null else values(i)

  override def toArray: Array[Any] = values.clone()

  override def copy(): GenericSeries = this

}

class GenericSeriesWithSchema(values: Array[Any], override val schema: StructType) extends GenericSeries(values) {

  /** No-arg constructor for serialization. */
  protected def this() = this(null, null)

  override def fieldIndex(name: String): Int = schema.getFieldIndex(name).getOrElse(-1)
}

object NullSeries extends Series {

  /** Number of elements in the Series. */
  override def length: Int = 0

  /**
   * Returns the value at position i. If the value is null, null is returned.
   */
  override def get(i: Int): Any = null

  /**
   * Make a copy of the current [[Series]] object.
   */
  override def copy(): Series = NullSeries
}


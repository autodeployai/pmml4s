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
import scala.collection.mutable.HashMap
import scala.reflect.ClassTag

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
  def unapplySeq(row: Series): Some[Seq[DataVal]] = Some(row.toSeq)

  /**
   * This method can be used to construct a [[Series]] with the given values.
   */
  def apply(values: Any*): Series = new GenericSeries(values.map(DataVal.from).toArray)

  /**
   * This method can be used to construct a [[Series]] from a [[Seq]] of values.
   */
  def fromSeq(values: Seq[DataVal]): Series = new GenericSeries(values.toArray)
  
  def fromArray[T](values: Array[T]): Series = {
    new GenericSeries(values.map(x => DataVal.from(x)))
  }

  def fromSeq(values: Seq[DataVal], schema: StructType): Series = new GenericSeriesWithSchema(values.toArray, schema)

  def fromArray[T](values: Array[T], schema: StructType): Series = {
    new GenericSeriesWithSchema(values.zip(schema).map(x => Utils.toDataVal(x._1, x._2.dataType)), schema)
  }

  def fromSplit(columns: Seq[String], values: Seq[DataVal]): Series = {
    require(values.size == columns.size, s"Should be same length, got columns=${columns.size} and values=${values.size}.")

    val fields = columns.map(x => StructField(x, UnresolvedDataType)).toArray
    new GenericSeriesWithSchema(values.toArray, StructType(fields))
  }
  
  def fromMap(map: Map[String, Any]): Series = {
    val values = new Array[DataVal](map.size)
    val fields = new Array[StructField](map.size)

    var i = 0
    map.foreach(
      pair => {
        val dataType = Utils.inferDataType(pair._2)
        fields(i) = StructField(pair._1, dataType)
        values(i) = Utils.toDataVal(pair._2, dataType)
        i += 1
      }
    )
    new GenericSeriesWithSchema(values, StructType(fields))
  }

  def fromMap(map: java.util.Map[String, Any]): Series = {
    val values = new Array[DataVal](map.size)
    val fields = new Array[StructField](map.size)

    var i = 0
    val it = map.entrySet().iterator()
    while (it.hasNext) {
      val pair = it.next()
      val dataType = Utils.inferDataType(pair.getValue)
      fields(i) = StructField(pair.getKey, dataType)
      values(i) = Utils.toDataVal(pair.getValue, dataType)
      i += 1
    }
    new GenericSeriesWithSchema(values, StructType(fields))
  }

  def fromMap(map: Map[String, Any], schema: StructType): Series = {
    val len = schema.length
    val values = new Array[DataVal](len)
    var i = 0
    while (i < len) {
      val f = schema(i)
      val v = map.get(f.name).orNull
      values(i) = if (v != null) Utils.toDataVal(v, f.dataType) else DataVal.NULL
      i += 1
    }
    new GenericSeriesWithSchema(values, schema)
  }

  def fromMap(map: java.util.Map[String, Any], schema: StructType): Series = {
    val len = schema.length
    val values = new Array[DataVal](len)
    var i = 0
    while (i < len) {
      val f = schema(i)
      val v = map.get(f.name)
      values(i) = if (v != null) Utils.toDataVal(v, f.dataType) else DataVal.NULL
      i += 1
    }
    new GenericSeriesWithSchema(values, schema)
  }

  def fromMap(map: JsObject, schema: StructType): Series = {
    val len = schema.length
    val values = new Array[DataVal](len)
    var i = 0
    while (i < len) {
      val f = schema(i)
      val v = map.fields.get(f.name).orNull
      values(i) = if (v != null) Utils.toDataVal(v.convertTo, f.dataType) else DataVal.NULL
      i += 1
    }
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
  val empty: Series = apply()
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
  def apply(i: Int): DataVal = get(i)

  /**
   * Returns the value at position i. If the value is null, null is returned.
   */
  def get(i: Int): DataVal

  /**
   * Returns the value of a given fieldName.
   */
  def get(fieldName: String): DataVal = get(fieldIndex(fieldName))

  /** Checks whether the value at position i is null. */
  def isNullAt(i: Int): Boolean = Utils.isNull(get(i))

  /**
   * Checks whether the value at position i is missing (null or Double.NaN) if the position is valid,
   * otherwise treated as missing too.
   */
  def isMissingAt(i: Int): Boolean = Utils.isMissing(get(i))

  def getDouble(i: Int): Double = get(i).toDouble

  def getString(i: Int): String = get(i).toString

  def getLong(i: Int): Long = get(i).toLong

  def getInt(i: Int): Int = get(i).toInt

  def getBoolean(i: Int): Boolean = get(i).toBool

  def toSeq: Seq[DataVal] = toArray.toSeq

  def asSeq: Seq[Any] = asArray.toSeq

  def toArray: Array[DataVal] = {
    val n = length
    val values = new Array[DataVal](n)
    var i = 0
    while (i < n) {
      values.update(i, get(i))
      i += 1
    }
    values
  }

  def asArray: Array[Any] = toArray.map(_.toVal)

  def toMap: Map[String, DataVal] = {
    val len = size
    val result = new mutable.HashMap[String, DataVal]()
    result.sizeHint(len)
    var i = 0
    while (i < len) {
      result.put(if (schema != null) schema.fieldName(i) else i.toString, get(i))
      i += 1
    }
    result.toMap
  }

  def asMap: Map[String, Any] = toMap.map(entry => (entry._1, entry._2.toVal))

  def toJavaMap: java.util.Map[String, DataVal] = {
    val len = size
    val result = new util.HashMap[String, DataVal](len)
    var i = 0
    while (i < len) {
      result.put(if (schema != null) schema.fieldName(i) else i.toString, get(i))
      i += 1
    }
    result
  }

  def asJavaMap: java.util.Map[String, Any] = {
    val len = size
    val result = new util.HashMap[String, Any](len)
    var i = 0
    while (i < len) {
      result.put(if (schema != null) schema.fieldName(i) else i.toString, get(i).toVal)
      i += 1
    }
    result
  }

  def toPairSeq: Seq[(String, DataVal)] = {
    if (schema != null) {
      schema.fieldNames.zip(toArray).toSeq
    } else {
      0 until size map (_.toString) zip toArray
    }
  }

  def asPairSeq: Seq[(String, Any)] = toPairSeq.map(entry => (entry._1, entry._2.toVal))

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
    JsObject(asMap.map(x => (x._1, x._2.toJson)))
  } else {
    JsArray(asArray.map(x => x.toJson).toVector)
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
class GenericSeries(val values: Array[DataVal]) extends Series {

  def this() = this(Array.empty[DataVal])

  def this(size: Int) = this(new Array[DataVal](size))

  override val length: Int = values.length

  override def get(i: Int): DataVal = if (i < 0 || i >= values.length) DataVal.NULL else values(i)

  @inline
  override def apply(i: Int): DataVal = values(i)

  override def toArray: Array[DataVal] = values.clone()

  override def copy(): GenericSeries = this
}

class GenericSeriesWithSchema(values: Array[DataVal], override val schema: StructType) extends GenericSeries(values) {

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
  override def get(i: Int): DataVal = NullVal

  /**
   * Make a copy of the current [[Series]] object.
   */
  override def copy(): Series = NullSeries
}


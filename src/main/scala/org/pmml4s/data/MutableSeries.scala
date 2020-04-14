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
package org.pmml4s.data

import org.pmml4s.common.{Clearable, StructType}

/**
 * A basic trait allows the values for each column to be updated.
 */
trait MutableSeries extends Series with Clearable with java.io.Serializable {
  def setNullAt(i: Int): Unit

  def update(i: Int, value: Any): Unit

  def setBoolean(i: Int, value: Boolean): Unit = {
    update(i, value)
  }

  def setInt(i: Int, value: Int): Unit = {
    update(i, value)
  }

  def setLong(i: Int, value: Long): Unit = {
    update(i, value)
  }

  def setFloat(i: Int, value: Float): Unit = {
    update(i, value)
  }

  def setDouble(i: Int, value: Double): Unit = {
    update(i, value)
  }

  def toSeries: Series

  override def clear(): Unit = {
    var i = 0
    while (i < length) {
      setNullAt(i)
      i += 1
    }
  }
}

class GenericMutableSeries(values: Array[Any]) extends MutableSeries {
  /** No-arg constructor for serialization. */
  protected def this() = this(null)

  def this(size: Int) = this(new Array[Any](size))

  override def setNullAt(i: Int): Unit = {
    values(i) = null
  }

  override def update(i: Int, value: Any): Unit = {
    values(i) = value
  }

  override def length: Int = values.length

  override def get(i: Int): Any = values(i)

  override def toSeq: Seq[Any] = values.toSeq

  override def copy(): GenericSeries = new GenericSeries(values.clone())

  override def toSeries: Series = new GenericSeries(values)
}

class GenericMutableSeriesWithSchema(values: Array[Any], override val schema: StructType) extends GenericMutableSeries(values) {

  /** No-arg constructor for serialization. */
  protected def this() = this(null, null)

  def this(size: Int, schema: StructType) = this(new Array[Any](size), schema)

  override def fieldIndex(name: String): Int = schema.fieldIndex(name)

  override def toSeries: Series = new GenericSeriesWithSchema(values, schema)
}

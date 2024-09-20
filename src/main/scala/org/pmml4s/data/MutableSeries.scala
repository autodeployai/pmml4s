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

import scala.reflect.ClassTag

/**
 * A basic trait allows the values for each column to be updated.
 */
trait MutableSeries extends Series with Clearable with java.io.Serializable {
  def setNullAt(i: Int): Unit

  def update(i: Int, value: DataVal): Unit

  def update(i: Int, value: String): Unit = {
    update(i, DataVal.from(value))
  }

  def setBoolean(i: Int, value: Boolean): Unit = {
    update(i, DataVal.from(value))
  }

  def setInt(i: Int, value: Int): Unit = {
    update(i, DataVal.from(value))
  }

  def setLong(i: Int, value: Long): Unit = {
    update(i, DataVal.from(value))
  }

  def setFloat(i: Int, value: Float): Unit = {
    update(i, DataVal.from(value))
  }

  def setDouble(i: Int, value: Double): Unit = {
    update(i, DataVal.from(value))
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

class GenericMutableSeries(values: Array[DataVal]) extends MutableSeries {
  /** No-arg constructor for serialization. */
  protected def this() = this(null)

  def this(size: Int) = this(new Array[DataVal](size))

  override def setNullAt(i: Int): Unit = {
    values(i) = DataVal.NULL
  }

  override def update(i: Int, value: DataVal): Unit = {
    values(i) = value
  }

  override val length: Int = values.length

  override def get(i: Int): DataVal = values(i)

  override def toArray: Array[DataVal] = values.clone()

  override def copy(): GenericSeries = new GenericSeries(values.clone())

  override def toSeries: Series = new GenericSeries(values)

  override def clear(): Unit = {
    java.util.Arrays.fill(values.asInstanceOf[Array[java.lang.Object]], DataVal.NULL)
  }
}

class GenericMutableSeriesWithSchema(values: Array[DataVal], override val schema: StructType) extends GenericMutableSeries(values) {

  /** No-arg constructor for serialization. */
  protected def this() = this(null, null)

  def this(size: Int, schema: StructType) = this(new Array[DataVal](size), schema)

  override def fieldIndex(name: String): Int = schema.fieldIndex(name)

  override def toSeries: Series = new GenericSeriesWithSchema(values, schema)
}

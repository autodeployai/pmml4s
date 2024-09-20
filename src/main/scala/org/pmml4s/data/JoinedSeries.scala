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

import org.pmml4s.common.{StructField, StructType}

import scala.reflect.ClassTag

/**
 * A wrapper that makes two series appear as a single concatenated series.
 */
class JoinedSeries(val series1: Series, val series2: Series) extends Series {
  private val length1 = series1.length
  private val length2 = series2.length

  def withLeft(left: Series): JoinedSeries = {
    new JoinedSeries(left, series2)
  }

  def withRight(right: Series): JoinedSeries = {
    new JoinedSeries(series1, right)
  }

  override def toArray: Array[DataVal] = {
    val values = new Array[DataVal](length)
    Array.copy(series1.toArray, 0, values, 0, length1)
    Array.copy(series2.toArray, 0, values, length1, length2)
    values
  }

  override val schema: StructType = if (series1.schema != null && series2.schema != null) {
    val len1 = series1.schema.fields.length
    val len2 = series2.schema.fields.length
    val fields = new Array[StructField](len1 + len2)
    System.arraycopy(series1.schema.fields, 0, fields, 0, len1)
    System.arraycopy(series2.schema.fields, 0, fields, len1, len2)
    StructType(fields)
  } else null

  override val length: Int = series1.length + series2.length

  /**
   * Returns the index of a given field name.
   */
  override def fieldIndex(name: String): Int = if (schema != null) {
    schema.getFieldIndex(name).getOrElse(-1)
  } else -1

  @inline
  override def get(i: Int): DataVal =
    if (i < length1) series1(i) else series2(i - length1)

  override def anyNull: Boolean = series1.anyNull || series2.anyNull

  override def anyMissing: Boolean = series1.anyMissing || series2.anyMissing

  override def copy(): Series = {
    val copy1 = series1.copy()
    val copy2 = series2.copy()
    new JoinedSeries(copy1, copy2)
  }

  override def toString: String = {
    // Make sure toString never throws NullPointerException.
    if ((series1 eq null) && (series2 eq null)) {
      "[ empty series ]"
    } else if (series1 eq null) {
      series2.toString
    } else if (series2 eq null) {
      series1.toString
    } else {
      s"{${series1.toString} + ${series2.toString}}"
    }
  }
}

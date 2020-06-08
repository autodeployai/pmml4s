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

import org.pmml4s.common.StructType

/**
 * A wrapper that makes two series appear as a single concatenated series.
 */
class JoinedSeries(val series1: Series, val series2: Series) extends Series {

  def withLeft(left: Series): JoinedSeries = {
    new JoinedSeries(left, series2)
  }

  def withRight(right: Series): JoinedSeries = {
    new JoinedSeries(series1, right)
  }

  override def toSeq: Seq[Any] = {
    series1.toSeq ++ series2.toSeq
  }

  override val schema: StructType = if (series1.schema != null && series2.schema != null) {
    StructType(series1.schema.fields ++ series2.schema.fields)
  } else null

  override val length: Int = series1.length + series2.length

  /**
   * Returns the index of a given field name.
   */
  override def fieldIndex(name: String): Int = if (schema != null) {
    schema.getFieldIndex(name).getOrElse(-1)
  } else -1

  override def get(i: Int): Any =
    if (i < series1.length) series1.get(i) else series2.get(i - series1.length)

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

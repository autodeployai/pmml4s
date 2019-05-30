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

import scala.collection.mutable

class CombinedSeries(val rows: Array[Series]) extends Series {
  val individualRows: Array[Series] = CombinedSeries.expand(rows)
  private[this] val indices: Array[Int] = {
    val res = mutable.ArrayBuilder.make[Int]
    res.sizeHint(individualRows.length + 1)
    var pos = 0
    res += pos
    for (r <- individualRows) {
      pos += r.length
      res += pos
    }
    res.result()
  }

  override val schema: StructType = if (rows.forall(_.schema != null)) {
    StructType(rows.map(_.schema.fields).flatten)
  } else null

  override val length: Int = indices(indices.length - 1)

  override def toArray: Array[Any] = {
    val result = new Array[Any](length)
    var i = 0
    for (r <- individualRows) {
      Array.copy(r.toArray, 0, result, i, r.length)
      i += r.length
    }
    result
  }

  def index(i: Int): (Series, Int) = {
    val len = individualRows.length

    if (len == 0) {
      (null, -1)
    } else if (len == 1) {
      (individualRows(0), i)
    } else if (len == 2) {
      if (i < individualRows(0).length) {
        (individualRows(0), i)
      } else {
        (individualRows(1), i - individualRows(0).length)
      }
    } else {
      var low = 0
      var high = indices.length - 1
      while (low <= high) {
        val mid = (low + high) >>> 1
        val midVal = indices(mid)

        if (i < midVal) {
          if (i >= indices(mid - 1)) {
            return (individualRows(mid - 1), i - indices(mid - 1))
          } else {
            high = mid - 1
          }
        } else if (i > midVal) {
          if (i < indices(mid + 1)) {
            return (individualRows(mid), i - midVal)
          } else {
            low = mid + 1
          }
        } else {
          return (individualRows(mid), i - midVal)
        }
      }

      (null, -1)
    }
  }

  def this() = {
    this(Array.empty)
  }

  /**
   * Returns the value at position i. If the value is null, null is returned.
   */
  override def get(i: Int): Any = {
    val (series, pos) = index(i)
    if (pos != -1) {
      series.get(pos)
    } else {
      throw new ArrayIndexOutOfBoundsException
    }
  }

  /**
   * Make a copy of the current [[Series]] object.
   */
  override def copy(): Series = new CombinedSeries(rows)

  def :+(series: Series): CombinedSeries = new CombinedSeries(rows :+ series)

  def +:(series: Series): CombinedSeries = new CombinedSeries(series +: rows)

  def last: Series = if (individualRows.length > 0) individualRows(individualRows.length - 1) else null

  def header: Series = if (individualRows.length > 0) individualRows(0) else null
}

object CombinedSeries {
  def expand(rows: Seq[Series]): Array[Series] = {
    val res = mutable.ArrayBuilder.make[Series]
    for (s <- rows) {
      s match {
        case x: CombinedSeries => res ++= expand(x.individualRows)
        case _                 => if (s.length > 0) {
          res += s
        }
      }
    }
    res.result()
  }
}
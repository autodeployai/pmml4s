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

import org.pmml4s.util.Utils

trait DSeries {

  def apply(i: Int): Double = get(i)

  def get(i: Int): Double

  def length: Int

  /** Checks whether the value at position i is null. */
  def isNullAt(i: Int): Boolean = java.lang.Double.isNaN(get(i))

  def isMissingAt(i: Int): Boolean = i < 0 || java.lang.Double.isNaN(get(i))

  def toArray: Array[Double] = {
    val n = length
    val values = new Array[Double](n)
    var i = 0
    while (i < n) {
      values.update(i, get(i))
      i += 1
    }
    values
  }

  def toSeq: Seq[Double] = toArray.toSeq

  /**
   * Make a copy of the current [[DSeries]] object.
   */
  def copy(): DSeries

}

object DSeries {

  /**
   * This method can be used to extract fields from a [[DSeries]] object in a pattern match.
   */
  def unapplySeq(series: DSeries): Some[Seq[Double]] = Some(series.toSeq)

  /**
   * This method can be used to construct a [[DSeries]] with the given values.
   */
  def apply(values: Double*): DSeries = new GenericDSeries(values.toArray)

  /**
   * This method can be used to construct a [[DSeries]] from a [[Seq]] of values.
   */
  def fromSeq(values: Seq[Double]): DSeries = new GenericDSeries(values.toArray)

  def fromArray(values: Array[Double]): DSeries = new GenericDSeries(values)

  def fromTuple(tuple: Product): DSeries = fromSeq(tuple.productIterator.toSeq.map(x => Utils.toDouble(x)))

  /**
   * Merge multiple rows into a single series, one after another.
   */
  def merge(rows: DSeries*): DSeries = {
    new GenericDSeries(rows.flatMap(_.toSeq).toArray)
  }

  /** Returns an empty row. */
  val empty: DSeries = apply()
}

class GenericDSeries(val values: Array[Double]) extends DSeries {

  def this() = this(Array.empty[Double])

  def this(size: Int) = this(new Array[Double](size))

  override def length: Int = values.length

  override def get(i: Int): Double = values(i)

  override def toArray: Array[Double] = values.clone()

  override def copy(): GenericDSeries = this
}

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
package org.pmml4s.common

import java.util

import scala.reflect.ClassTag

trait Vector[@specialized(Int, Long, Float, Double) V] extends java.io.Serializable {

  def size: Int

  def apply(i: Int): V

  def length: Int = size

  def toArray(implicit cm: ClassTag[V]) = {
    val result = new Array[V](length)
    var i = 0
    while (i < length) {
      result(i) = apply(i)
      i += 1
    }
    result
  }
}

class DenseVector[@specialized(Int, Long, Float, Double) V](val values: Array[V]) extends Vector[V] {
  override def size: Int = values.length

  override def apply(i: Int): V = values(i)

  override def toArray(implicit cm: ClassTag[V]): Array[V] = values
}

class SparseVector[@specialized(Int, Long, Float, Double) V](
                                                              override val size: Int,
                                                              val indices: Array[Int],
                                                              val values: Array[V],
                                                              val default: V) extends Vector[V] {
  override def apply(i: Int): V = {
    val idx = index(i)
    if (idx >= 0) values(idx) else default
  }

  protected final def index(i: Int): Int = {
    util.Arrays.binarySearch(indices, 0, indices.length, i)
  }
}
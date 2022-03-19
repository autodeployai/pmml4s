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

import org.pmml4s.common.Vector

/**
 * Various utility methods about mathematics
 */
object MathUtils {

  def product(x1: Array[Double], x2: Array[Double]): Double = {
    var res = 0.0
    val len = Math.min(x1.length, x2.length)
    var i = 0
    while (i < len) {
      res += x1(i) * x2(i)
      i += 1
    }
    res
  }

  def product(x1: Array[Double], x2: Vector[Double]): Double = {
    var res = 0.0
    val len = Math.min(x1.length, x2.length)
    var i = 0
    while (i < len) {
      res += x1(i) * x2(i)
      i += 1
    }
    res
  }

  def median(x: Array[Double]): Double = {
    val sorted = x.sorted
    val n = sorted.size
    if (n % 2 == 0) {
      (sorted(n / 2) + sorted(n / 2 - 1)) / 2.0
    } else {
      sorted(n / 2)
    }
  }

  def weightedMedian(x: Array[Double], w: Array[Double]): Double = {
    val sorted = x.zip(w).sortBy(_._1)
    val s = w.sum
    val s2 = s / 2
    var sum = s - sorted(0)._2
    var k = 0
    while (sum > s2) {
      k += 1
      sum -= sorted(k)._2
    }

    sorted(k)._1
  }

  def filterMissing(x: Seq[Double]): Seq[Double] = {
    x.filter(x => x == x)
  }
}

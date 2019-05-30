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

/**
 * Various utility methods operate all elements in array
 */
object ArrayUtils {
  val emptyAnyRefArray = new Array[AnyRef](0)
  val emptyAnyArray = new Array[Any](0)
  val emptyStringArray: Array[String] = new Array[String](0)

  def toDouble(a: Array[String]): Array[Double] = {
    val res = new Array[Double](a.length)
    for (i <- 0 until a.length) {
      res(i) = a(i).toDouble
    }
    res
  }

  def toInt(a: Array[String]): Array[Int] = {
    val res = new Array[Int](a.length)
    for (i <- 0 until a.length) {
      res(i) = a(i).toInt
    }
    res
  }

  def toLong(a: Array[String]): Array[Long] = {
    val res = new Array[Long](a.length)
    for (i <- 0 until a.length) {
      res(i) = a(i).toLong
    }
    res
  }

  def subArray(a: Array[_], indices: Array[Int]): Array[_] = {
    indices.map(a(_))
  }
}

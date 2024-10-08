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
package org.pmml4s.util

import org.pmml4s.data.DataVal

import scala.reflect.ClassTag

/**
 * Various utility methods operate all elements in array
 */
object ArrayUtils {
  val emptyAnyRefArray = new Array[AnyRef](0)
  val emptyAnyArray = new Array[Any](0)
  val emptyStringArray: Array[String] = new Array[String](0)
  val emptyDataValArray: Array[DataVal] = new Array[DataVal](0)

  def toDouble(a: Array[String]): Array[Double] = {
    val res = new Array[Double](a.length)
    var i = 0
    while (i < a.length) {
      res(i) = StringUtils.asDouble(a(i))
      i += 1
    }
    res
  }

  def toInt(a: Array[String]): Array[Int] = {
    val res = new Array[Int](a.length)
    var i = 0
    while (i < a.length) {
      res(i) = a(i).toInt
      i += 1
    }
    res
  }

  def toLong(a: Array[String]): Array[Long] = {
    val res = new Array[Long](a.length)
    var i = 0
    while (i < a.length) {
      res(i) = a(i).toLong
      i += 1
    }
    res
  }

  def subArray[T: ClassTag](a: Array[T], indices: Array[Int]): Array[_] = {
    indices.map(a(_))
  }
}


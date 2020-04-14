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

import org.pmml4s.common.Property.Property
import org.pmml4s.util.Utils

import scala.collection.mutable

class Value(
             val value: String,
             val displayValue: Option[String] = None,
             val property: Property = Property.valid) extends PmmlElement

object Value {
  def distinguish(values: Array[Value], dataType: DataType): (Array[Any], Set[Any], Set[Any], Map[Any, String]) = {
    val valid = mutable.ArrayBuilder.make[Any]
    valid.sizeHint(values)
    val invalid = new mutable.HashSet[Any]()
    val missing = new mutable.HashSet[Any]()
    val labels = new mutable.HashMap[Any, String]()

    for (v <- values) {
      val a = Utils.toVal(v.value, dataType)
      v.property match {
        case Property.valid   => valid += a
        case Property.invalid => invalid += a
        case Property.missing => missing += a
      }

      if (v.displayValue.isDefined)
        labels += ((a, v.displayValue.get))
    }

    (valid.result(), invalid.toSet, missing.toSet, labels.toMap)
  }

  def labels(values: Array[Value]): Map[Any, String] = {
    val result = new mutable.HashMap[Any, String]()
    for (v <- values) {
      if (v.displayValue.isDefined)
        result += ((v.value, v.displayValue.get))
    }

    result.toMap
  }
}

/**
 *  - Valid value: A value which is neither missing nor invalid.
 *  - Invalid value: The input value is not missing but it does not belong to a certain value range. The range of valid
 * values can be defined for each field.
 *  - Missing value: Input value is missing, for example, if a database column contains a null value. It is possible to
 * explicitly define values which are interpreted as missing values.
 */
object Property extends Enumeration {
  type Property = Value
  val valid, invalid, missing = Value
}

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
package org.pmml4s.transformations

import org.pmml4s.common.{DataType, Interval, PmmlElement}
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.Field
import org.pmml4s.util.Utils

import scala.collection.mutable

/**
 * Discretization of numerical input fields is a mapping from continuous to discrete values using intervals.
 */
class Discretize(
                  val discretizeBins: Array[DiscretizeBin],
                  val field: Field,
                  val mapMissingTo: Option[DataVal],
                  val defaultValue: Option[DataVal],
                  val dataType: Option[DataType]) extends FieldExpression {
  private val replacement: DataVal = mapMissingTo.getOrElse(DataVal.NULL)

  override def eval(series: Series): DataVal = {
    val res = super.eval(series)
    if (Utils.isMissing(res)) {
      replacement
    } else {
      evaluate(res.toDouble)
    }
  }

  override def eval(x: DataVal): DataVal = {
    if (Utils.isMissing(x)) {
      replacement
    } else {
      evaluate(x.toDouble)
    }
  }

  override val categories: Array[DataVal] = {
    val c = new mutable.LinkedHashSet[DataVal]()
    discretizeBins.foreach { e =>
      c += e.binValue
    }

    mapMissingTo.foreach(c += _)
    defaultValue.foreach(c += _)

    c.toArray
  }

  private def evaluate(value: Double): DataVal = {
    val one = discretizeBins.find(x => x.interval.contains(value))
    one.map(_.binValue).getOrElse(replacement)
  }
}

class DiscretizeBin(
                     val interval: Interval,
                     val binValue: DataVal) extends PmmlElement

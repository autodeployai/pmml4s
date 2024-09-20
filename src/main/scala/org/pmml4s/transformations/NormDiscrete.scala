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

import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.Field
import org.pmml4s.util.Utils

/**
 * Encode string values into numeric values in order to perform mathematical computations. For example, regression and
 * neural network models often split categorical and ordinal fields into multiple dummy fields. This kind of
 * normalization is supported in PMML by the element NormDiscrete.
 *
 * An element (f, v) defines that the unit has value 1.0 if the value of input field f is v, otherwise it is 0.
 *
 * The set of NormDiscrete instances which refer to a certain input field define a fan-out function which maps a single
 * input field to a set of normalized fields.
 *
 * If the input value is missing and the attribute mapMissingTo is not specified then the result is a missing value as
 * well. If the input value is missing and the attribute mapMissingTo is specified then the result is the value of the
 * attribute mapMissingTo.
 */
class NormDiscrete(
                    val field: Field,
                    val value: DataVal,
                    val mapMissingTo: Option[Double]) extends FieldExpression {
  private val replacement: DataVal = mapMissingTo.map(DataVal.from).getOrElse(DataVal.NaN)

  override def eval(series: Series): DataVal = {
    val res = super.eval(series)
    if (Utils.isMissing(res)) {
      replacement
    } else {
      if (res == value) DataVal.`1.0` else DataVal.`0.0`
    }
  }
}

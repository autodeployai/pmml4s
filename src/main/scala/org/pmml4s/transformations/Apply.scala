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
package org.pmml4s.transformations

import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.InvalidValueTreatment.InvalidValueTreatment
import org.pmml4s.metadata.{Field, InvalidValueTreatment}
import org.pmml4s.util.Utils

/**
 * Apply defines the application of a function. The function itself is identified by name with the function attribute.
 * The actual parameters of the function application are given in the content of the element. Each actual argument value
 * is given by an EXPRESSION and are mapped by position to the formal parameters in the corresponding function
 * definition.
 */
class Apply(
             val function: Function,
             val children: Array[Expression],
             val mapMissingTo: Option[DataVal] = None,
             val defaultValue: Option[DataVal] = None,
             val invalidValueTreatment: InvalidValueTreatment = InvalidValueTreatment.returnInvalid) extends Expression {

  /** Returns the result of evaluating this expression on a given input Series */
  override def eval(series: Series): DataVal = {
    val values = new Array[DataVal](children.length)
    var i = 0
    while (i < values.length) {
      val child = children(i)
      val res = child.eval(series)
      if (Utils.isMissing(res)) {
        if (mapMissingTo.isDefined)
          return mapMissingTo.get
      }
      values(i) = res
      i += 1
    }

    val res = function(values: _*)
    if (Utils.isMissing(res)) {
      if (defaultValue.isDefined)
        defaultValue.get
      else {
        import InvalidValueTreatment._
        invalidValueTreatment match {
          case `returnInvalid` | `asIs` => res
          case `asMissing`              => DataVal.NULL
        }
      }
    } else res
  }

  override def getDataField: Option[Field] = {
    for (child <- children) {
      val res = child.getDataField
      if (res.isDefined)
        return res
    }
    None
  }
}


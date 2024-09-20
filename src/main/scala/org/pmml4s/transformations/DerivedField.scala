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

import org.pmml4s.common._
import org.pmml4s.data.{CombinedSeries, DataVal, MutableSeries, NullVal, Series}
import org.pmml4s.metadata.{DataField, Field, FieldType}
import org.pmml4s.util.Utils

/**
 * Provides a common element for the various mappings. They can also appear at several places in the definition of
 * specific models such as neural network or Naive Bayes models. Transformed fields have a name such that statistics
 * and the model can refer to these fields.
 */
class DerivedField(
                    override val name: String,
                    override val displayName: Option[String],
                    override val dataType: DataType,
                    override val opType: OpType,
                    override val values: Array[Value],
                    val expr: Expression) extends DataField(name, displayName, dataType, opType, values)
  with Expression {

  def this(name: String, displayName: Option[String], dataType: DataType, opType: OpType, expr: Expression) = {
    this(name, displayName, dataType, opType, Array.empty, expr)
  }

  def this(name: String, expr: Expression) = {
    this(name, None, UnresolvedDataType, OpType.typeless, expr)
  }
  
  /** Field type. */
  override def fieldType: FieldType = FieldType.DerivedField

  /** Retrieve its value from the specified series, return null if missing */
  override def get(series: Series): DataVal = {
    val result = super.get(series)
    if (result == null) {
      eval(series)
    } else {
      result
    }
  }

  def write(series: Series, mutableSeries: MutableSeries, pos: Int): DataVal = {
    val res = eval(series)
    mutableSeries.update(pos, res)
    res
  }

  override def eval(series: Series): DataVal = {
    val result = super.get(series)

    if (result == null || result.isMissing) {
      val value = expr.eval(series)

      // Convert the result based on the field's data type
      if (value == null || value.isMissing) {
        NullVal
      } else if (value.dataType != dataType) {
        Utils.toDataVal(value, dataType)
      } else value
    } else result
  }

  override def children: Array[Expression] = expr.children

  override def deeval(input: DataVal): DataVal = expr.deeval(input)

  override def getDataField: Option[Field] = expr.getDataField
}


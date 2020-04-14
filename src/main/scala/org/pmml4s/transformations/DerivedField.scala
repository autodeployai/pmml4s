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
import org.pmml4s.data.{CombinedSeries, MutableSeries, Series}
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
  with Expression
  with Clearable {
  @transient private[this] var value: Any = _
  @transient private[this] var evaluated: Boolean = false
  @transient private[this] var written: Boolean = false

  def this(name: String, displayName: Option[String], dataType: DataType, opType: OpType, expr: Expression) {
    this(name, displayName, dataType, opType, Array.empty, expr)
  }

  def this(name: String, expr: Expression) {
    this(name, None, UnresolvedDataType, OpType.typeless, expr)
  }
  
  /** Field type. */
  override def fieldType = FieldType.DerivedField

  /** Retrieve its value from the specified series, return null if missing */
  override def get(series: Series): Any = {
    if (!written) {
      series match {
        case cs: CombinedSeries => {
          val last = cs.last
          write(series, last.asInstanceOf[MutableSeries], index + (cs.length - last.length))
        }
        case _ => eval(series)
      }
    } else {
      super.get(series)
    }
  }

  def write(series: Series, mutableSeries: MutableSeries, pos: Int): Any = {
    val res = eval(series)
    mutableSeries.update(pos, res)
    written = true
    res
  }

  override def clear(): Unit = {
    evaluated = false
    written = false
    value = null
  }

  override def eval(series: Series): Any = {
    if (!evaluated) {
      value = expr.eval(series)

      // Convert the result based on the field's data type
      value = Utils.orNull(value, Utils.toVal(value, dataType))
    }
    value
  }

  override def children: Array[Expression] = expr.children

  override def deeval(input: Any): Any = expr.deeval(input)

  override def getDataField: Option[Field] = expr.getDataField
}

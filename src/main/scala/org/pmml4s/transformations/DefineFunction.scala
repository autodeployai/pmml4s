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
import org.pmml4s.data.Series
import org.pmml4s.metadata.{AbstractField, Field, FieldScope, FieldType}

/**
 * Defines new (user-defined) functions as variations or compositions of existing functions or transformations. The
 * function's name must be unique and must not conflict with other function names, either defined by PMML or other
 * user-defined functions. The EXPRESSION in the content of DefineFunction is the function body that actually defines
 * the meaning of the new function. The function body must not refer to fields other than the parameter fields.
 */
class DefineFunction(
                      val name: String,
                      val parameterFields: Array[ParameterField],
                      val expr: Expression,
                      val opType: OpType,
                      val dataType: DataType) extends Function with HasOpType with HasDataType with PmmlElement {
  parameterFields.zipWithIndex.foreach(x => x._1.index = x._2)

  override def apply(parameters: Any*): Any = {
    val series = Series.fromSeq(parameters)
    expr.eval(series)
  }

  override def symbol: String = name

  def scope: FieldScope = new FieldScope {
    private lazy val nameToField = parameterFields.map(x => (x.name, x)).toMap

    override def getField(name: String): Option[Field] = nameToField.get(name)
  }
}

class ParameterField(val name: String, val opType: OpType, val dataType: DataType, displayName: Option[String] = None)
  extends AbstractField {

  /** Field type. */
  override def fieldType: FieldType = FieldType.ParameterField

  /** Retrieve its value from the specified series, return null if missing */
  override def get(series: Series): Any = if (index >= 0) series.get(index) else null
}

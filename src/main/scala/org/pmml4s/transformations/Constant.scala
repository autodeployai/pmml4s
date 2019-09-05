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

import org.pmml4s.common.DataType
import org.pmml4s.data.Series
import org.pmml4s.metadata.Field

/**
 * Constant values can be used in expressions which have multiple arguments. . The actual value of a constant is given
 * by the content of the element. For example, <Constant>1.05</Constant> represents the number 1.05. The dataType of
 * Constant can be optionally specified.
 */
class Constant(
                val value: Any,
                val dataType: Option[DataType] = None,
                val missing: Boolean = false) extends LeafExpression {
  override def eval(series: Series): Any = if (missing) null else value

  override def getDataField: Option[Field] = None
}

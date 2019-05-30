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
package org.pmml4s.metadata

import org.pmml4s.common.{DataType, OpType}
import org.pmml4s.data.Series

/**
 * Defines the wrapped field that contains an internal field acts all operations.
 */
class WrappedField(override val name: String) extends Field {
  var field: Field = _

  /** Display name of the field. None if it is not set. */
  override def displayName = field.displayName

  /** Attribute of the field. */
  override def attribute = field.attribute

  /** Index of the field in the input series. */
  override def index: Int = field.index

  /** Sets the index of this field. */
  override def index_=(i: Int): Unit = field.index_=(i)

  /** Tests if the field is referenced in the model element. */
  override def referenced = field.referenced

  /** Sets the referenced flag of the field. */
  override def referenced_=(r: Boolean): Unit = field.referenced_=(r)

  /** Field type. */
  override def fieldType: FieldType = FieldType.WrappedField

  /** Retrieve its value from the specified series, return null if missing */
  override def get(series: Series): Any = field.get(series)

  /** Returns the data type of field. */
  override def dataType: DataType = field.dataType

  /** Operational type. */
  override def opType: OpType = field.opType
}

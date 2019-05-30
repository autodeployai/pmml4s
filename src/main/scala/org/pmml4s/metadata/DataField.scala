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

import org.pmml4s.common._

/**
 * Defines a field as used in mining models. It specifies the types and value ranges.
 */
class DataField(
                 override val name: String,
                 override val displayName: Option[String],
                 override val dataType: DataType,
                 override val opType: OpType,
                 override val intervals: Seq[Interval] = Seq.empty,
                 val values: Seq[Value] = Seq.empty,
                 val taxonomy: Option[String] = None,
                 val isCyclic: Boolean = true
               ) extends AbstractField with PmmlElement {

  def this(name: String, displayName: Option[String], dataType: DataType, opType: OpType, values: Seq[Value]) {
    this(name, displayName, dataType, opType, Seq.empty[Interval], values)
  }

  def this(name: String, displayName: Option[String], dataType: DataType, opType: OpType, interval: Interval) {
    this(name, displayName, dataType, opType, Seq(interval), Seq.empty[Value])
  }

  def this(name: String, displayName: Option[String], dataType: DataType, opType: OpType, interval: Interval, values: Seq[Value]) {
    this(name, displayName, dataType, opType, Seq(interval), values)
  }

  /** An internal attribute could be changed. */
  _attribute = Attribute(dataType, opType, intervals, values)

  /** Field type. */
  override def fieldType: FieldType = FieldType.DataField
}

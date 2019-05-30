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

import org.pmml4s.data.Series

/**
 * Abstract class for field in a PMML with common implementations.
 */
abstract class AbstractField extends Field {

  /** Index of the field in the input series. */
  @transient protected var _index: Int = -1

  /** A flag if the field is referenced in the model. */
  @transient protected var _referenced: Boolean = false

  /** An internal attribute could be changed. */
  protected var _attribute: Attribute = Attribute(opType)

  /** Index of the field in the input series. */
  override def index: Int = _index

  /** Sets the index of this field. */
  override def index_=(i: Int): Unit = _index = i

  /** Tests if the field is referenced in the model element. */
  override def referenced = _referenced

  /** Sets the referenced flag of the field. */
  override def referenced_=(r: Boolean): Unit = _referenced = r

  /** Retrieve its value from the specified series, return null if missing */
  override def get(series: Series): Any = if (index >= 0) series.get(index) else null

  /** Attribute of the field. */
  override def attribute = _attribute

  /** Converts to an immutable attribute if it's mutable. */
  override def toImmutable(): this.type = {
    if (attribute.isMutable) {
      _attribute = toAttribute
    }
    this
  }
}

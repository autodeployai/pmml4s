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

import org.pmml4s.common.PmmlElement

/**
 * Contains definitions for fields as used in mining models. It specifies the types and value ranges.
 * These definitions are assumed to be independent of specific data sets as used for training or scoring a specific model.
 */
class DataDictionary(override val fields: Array[DataField]) extends Dictionary[DataField] with PmmlElement {

  def this() = this(Array.empty[DataField])

  /**
   * Returns a [[DataDictionary]] containing [[Field]]s of the given names, preserving the
   * original order of fields.
   *
   * @throws IllegalArgumentException if a field cannot be found for any of the given names
   */
  override def apply(names: Set[String]): DataDictionary = {
    val nonExistFields = names -- fieldNamesSet
    if (nonExistFields.nonEmpty) {
      throw new IllegalArgumentException(
        s"Field ${nonExistFields.mkString(",")} does not exist.")
    }
    // Preserve the original order of fields.
    DataDictionary(fields.filter(f => names.contains(f.name)))
  }
}

object DataDictionary {

  def apply(fields: Seq[DataField]): DataDictionary = new DataDictionary(fields.toArray)
}
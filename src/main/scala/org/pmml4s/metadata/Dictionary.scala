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

abstract class Dictionary[T <: Field] extends Seq[T] with HasField {

  def fields: Array[T]

  /** Returns all field names in an array. */
  def fieldNames: Array[String] = fields.map(_.name)

  protected lazy val fieldNamesSet: Set[String] = fieldNames.toSet
  protected lazy val nameToField: Map[String, T] = fields.map(f => f.name -> f).toMap
  protected lazy val nameToIndex: Map[String, Int] = fieldNames.zipWithIndex.toMap

  /**
   * Extracts the [[Field]] with the given name.
   *
   * @throws IllegalArgumentException if a field with the given name does not exist
   */
  def apply(name: String): T = {
    nameToField.getOrElse(name,
      throw new IllegalArgumentException(s"""Field "$name" does not exist."""))
  }

  def get(name: String): Option[T] = {
    nameToField.get(name)
  }

  /**
   * Returns a [[DataDictionary]] containing [[Field]]s of the given names, preserving the
   * original order of fields.
   *
   * @throws IllegalArgumentException if a field cannot be found for any of the given names
   */
  def apply(names: Set[String]): Dictionary[T]

  /**
   * Returns the index of a given field.
   *
   * @throws IllegalArgumentException if a field with the given name does not exist
   */
  def fieldIndex(name: String): Int = {
    nameToIndex.getOrElse(name,
      throw new IllegalArgumentException(s"""Field "$name" does not exist."""))
  }

  override def apply(fieldIndex: Int): T = fields(fieldIndex)

  override def length: Int = fields.length

  override def iterator: Iterator[T] = fields.iterator

  /**
   * Returns the field of a given name, None if a field with the given name does not exist
   */
  override def getField(name: String): Option[Field] = get(name)
}
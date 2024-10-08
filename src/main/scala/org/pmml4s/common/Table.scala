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
package org.pmml4s.common

import org.pmml4s.data.DataVal
import org.pmml4s.xml.ElemTags.{INLINE_TABLE, TABLE_LOCATOR}

sealed trait Table extends PmmlElement {
  def find(inputs: Map[String, DataVal], output: String): Option[DataVal]

  def apply(i: Int): Row

  def dim: (Int, Int)
}

object Table {
  val values: Set[String] = Set(TABLE_LOCATOR, INLINE_TABLE)

  def contains(s: String): Boolean = values.contains(s)
}

class InlineTable(val rows: Array[Row]) extends Table {
  override def find(inputs: Map[String, DataVal], output: String): Option[DataVal] = {
    val r = rows.find(x => inputs.forall(p => x.elements.get(p._1).contains(p._2)))
    if (r.isDefined) r.get.elements.get(output) else None
  }

  override def apply(i: Int): Row = rows(i)

  override def dim: (Int, Int) = (rows.length, if (rows.nonEmpty) rows.head.size else 0)
}

class TableLocator extends Table {
  override def find(inputs: Map[String, DataVal], output: String): Option[DataVal] = ???

  override def apply(i: Int): Row = ???

  override def dim: (Int, Int) = ???
}

class Row(val elements: Map[String, DataVal]) extends PmmlElement {
  def size: Int = elements.size

  def apply(name: String): DataVal = elements(name)

  def get(name: String): Option[DataVal] = elements.get(name)
}
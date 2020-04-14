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

import org.pmml4s.common.{DataType, PmmlElement, Table}
import org.pmml4s.data.Series
import org.pmml4s.metadata.Field

/**
 * Any discrete value can be mapped to any possibly different discrete value by listing the pairs of values. This list
 * is implemented by a table, so it can be given inline by a sequence of XML markups or by a reference to an external
 * table.
 */
class MapValues(
                 val fieldColumnPairs: Array[FieldColumnPair],
                 val table: Table,
                 val outputColumn: String,
                 val mapMissingTo: Option[Any],
                 val defaultValue: Option[Any],
                 val dataType: Option[DataType]) extends Expression {
  override def eval(series: Series): Any = {
    if (fieldColumnPairs.exists(x => x.field.isMissing(series))) {
      mapMissingTo.getOrElse(null)
    } else {
      val r = table.find(fieldColumnPairs.map(x => (x.column, series(x.field.index))).toMap, outputColumn)
      r.getOrElse(defaultValue.getOrElse(null))
    }
  }

  override def children: Array[Expression] =  fieldColumnPairs.map(x => new FieldRef(x.field))

  override def getDataField: Option[Field] = {
    // Returns the first one
    val field = fieldColumnPairs.head.field
    if (field.isDataField) Some(field) else {
      if (field.isDerivedField) field.asInstanceOf[DerivedField].getDataField else None
    }
  }
}

class FieldColumnPair(val field: Field, val column: String) extends PmmlElement

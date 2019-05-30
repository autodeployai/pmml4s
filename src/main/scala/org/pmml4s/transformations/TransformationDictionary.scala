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

import org.pmml4s.common.{PmmlElement, StructField, StructType, Transformer}
import org.pmml4s.data.{GenericMutableSeriesWithSchema, Series}
import org.pmml4s.metadata.{DataDictionary, Dictionary, Field}

/**
 * The TransformationDictionary allows for transformations to be defined once and used by any model element in the PMML
 * document.
 */
class TransformationDictionary(
                                override val fields: Array[DerivedField],
                                val defineFunctions: Array[DefineFunction]
                              ) extends Dictionary[DerivedField] with Transformer with FunctionProvider with PmmlElement {
  /** The schema of output. */
  lazy val outputSchema: StructType = StructType(fields.map { x => StructField(x.name, x.dataType) })
  private lazy val outputSeries = new GenericMutableSeriesWithSchema(fields.length, outputSchema)
  private lazy val nameToFunction: Map[String, DefineFunction] = defineFunctions.map(x => (x.name, x)).toMap

  def this(fields: Array[DerivedField]) = this(fields, Array.empty)

  def this() = this(Array.empty)

  /**
   * Returns a [[DataDictionary]] containing [[Field]]s of the given names, preserving the
   * original order of fields.
   *
   * @throws IllegalArgumentException if a field cannot be found for any of the given names
   */
  override def apply(names: Set[String]): TransformationDictionary = {
    val nonExistFields = names -- fieldNamesSet
    if (nonExistFields.nonEmpty) {
      throw new IllegalArgumentException(
        s"Field ${nonExistFields.mkString(",")} does not exist.")
    }
    // Preserve the original order of fields.
    new TransformationDictionary(fields.filter(f => names.contains(f.name)))
  }

  override def transform(series: Series): Series = {
    // Set correct indices for all fields before calculate their values.
    if (series.size != startPos) {
      startPos = series.size
      for (i <- 0 until length) {
        fields(i).index = startPos + i
      }
    }

    outputSeries.clear()
    val combinedSeries = Series.merge(series, outputSeries)
    for (i <- 0 until length) {
      fields(i).write(combinedSeries, outputSeries, i)
    }

    combinedSeries
  }

  override def getFunction(name: String): Option[DefineFunction] = nameToFunction.get(name)

  def referencedFields = fields.filter(_.referenced)

  /** Identifies the using start position of output series. */
  @transient private[this] var startPos = -1
}

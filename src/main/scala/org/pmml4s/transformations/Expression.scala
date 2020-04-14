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

import org.pmml4s.common.{Evaluator, PmmlElement}
import org.pmml4s.data.Series
import org.pmml4s.metadata.Field
import org.pmml4s.util.ArrayUtils
import org.pmml4s.xml.ElemTags._

/**
 * Trait of Expression that defines how the values of the new field are computed.
 */
trait Expression extends Evaluator with PmmlElement {

  /** Returns the result of evaluating this expression on a given input Series */
  override def eval(series: Series): Any

  def eval(value: Any): Any = new UnsupportedOperationException("Can not support to evaluate of a single value.")

  def deeval(value: Any): Any = new UnsupportedOperationException("Can not support de-normalized operation.")

  def children: Array[Expression]

  def getDataField: Option[Field]

  def categories: Array[Any] = ArrayUtils.emptyAnyArray
}

trait LeafExpression extends Expression {
  override final def children: Array[Expression] = Array.empty
}

trait UnaryExpression extends Expression {
  def child: Expression

  override final def children: Array[Expression] = Array(child)
}

trait FieldExpression extends UnaryExpression {

  def field: Field

  override def eval(series: Series): Any = {
    field.get(series)
  }

  override def child: FieldRef = new FieldRef(field)

  override def getDataField: Option[Field] = if (field.isDataField) Some(field) else {
    if (field.isDerivedField) field.asInstanceOf[DerivedField].getDataField else None
  }
}

trait NumericFieldExpression extends FieldExpression {
  override def eval(series: Series): Double = {
    field.getDouble(series)
  }
}

object Expression {
  val values = Set(CONSTANT, FIELD_REF, NORM_CONTINUOUS, NORM_DISCRETE, DISCRETIZE, MAP_VALUES, TEXT_INDEX, APPLY, AGGREGATE, LAG)

  def contains(s: String) = values.contains(s)
}

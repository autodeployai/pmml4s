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

import org.pmml4s.data.Series
import org.pmml4s.metadata.Field
import org.pmml4s.transformations.FieldRef
import org.pmml4s.util.Utils
import org.pmml4s.xml.ElemTags.{CATEGORICAL_PREDICTOR, NUMERIC_PREDICTOR, PREDICTOR_TERM}

sealed trait RegressionPredictor extends RegressionEvaluator {
  def eval(series: Series): Double
}

object RegressionPredictor {

  val values = Set(NUMERIC_PREDICTOR, CATEGORICAL_PREDICTOR, PREDICTOR_TERM)

  def contains(s: String) = values.contains(s)
}

/**
 * Defines a numeric independent variable. The list of valid attributes comprises the name of the variable,
 * the exponent to be used, and the coefficient by which the values of this variable must be multiplied.
 * Note that the exponent defaults to 1, hence it is not always necessary to specify. Also, if the input value
 * is missing, the result evaluates to a missing value.
 */
class NumericPredictor(val field: Field, val coefficient: Double, val exponent: Int = 1) extends RegressionPredictor {
  override def eval(series: Series): Double = if (field.isMissing(series)) Double.NaN else {
    val value = field.getDouble(series)
    if (exponent == 1) value * coefficient else Math.pow(value, exponent) * coefficient
  }
}

/**
 * Defines a categorical independent variable. The list of attributes comprises the name of the variable,
 * the value attribute, and the coefficient by which the values of this variable must be multiplied.
 */
class CategoricalPredictor(val field: Field, val coefficient: Double, val value: Any) extends RegressionPredictor {
  override def eval(series: Series): Double = if (field.isMissing(series) || field.get(series) != value) 0.0 else coefficient
}

/**
 * Contains one or more fields that are combined by multiplication. That is, this element supports interaction terms.
 * The type of all fields referenced within PredictorTerm must be continuous. Note that if the input value is missing,
 * the result evaluates to a missing value.
 */
class PredictorTerm(val name: Option[String], val coefficient: Double, val fields: Array[FieldRef]) extends RegressionPredictor {
  override def eval(series: Series): Double = {
    var result = coefficient
    for (f <- fields) {
      val r = f.eval(series)
      if (Utils.isMissing(r))
        return Double.NaN

      result *= Utils.toDouble(r)
    }
    result
  }
}

/**
 * Lists the values of all predictors or independent variables. If the model is used to predict a numerical field,
 * then there is only one RegressionTable and the attribute targetCategory may be missing. If the model is used to predict a categorical field,
 * then there are two or more RegressionTables and each one must have the attribute targetCategory defined with a unique value.
 */
class RegressionTable(val predictors: Array[RegressionPredictor], val intercept: Double, val targetCategory: Option[Any] = None) extends
  RegressionPredictor {
  override def eval(series: Series): Double = {
    var result = intercept
    for (predictor <- predictors) {
      val r = predictor.eval(series)
      if (Utils.isMissing(r))
        return Double.NaN

      result += r
    }

    result
  }
}


class FactorPredictor(override val field: Field, override val value: Any) extends CategoricalPredictor(field, 1.0, value)

class ContrastMatrixFactorPredictor(override val field: Field, override val value: Any, val categories: Map[Any, Int], val matrix: Matrix)
  extends FactorPredictor(field, value) {
  private val colIdx: Int = categories(value)

  override def eval(series: Series): Double = {
    if (field.isMissing(series)) 0.0 else {
      val rowIdx = categories(field.get(series))
      matrix(rowIdx, colIdx)
    }
  }
}

class CovariatePredictor(val field: Field, val multiplicity: Double) extends RegressionPredictor {
  override def eval(series: Series): Double = if (field.isMissing(series)) Double.NaN else {
    val value = field.getDouble(series)
    if (multiplicity == 1.0) value else Math.pow(value, multiplicity)
  }
}

class RegressionParameter(val factors: Array[FactorPredictor], val covariates: Array[CovariatePredictor]) extends RegressionPredictor {
  override def eval(series: Series): Double = {
    var res = 1.0
    if (factors.length > 0) {
      for (f <- factors) {
        val value = f.eval(series)
        if (value == 0.0)
          return 0.0
      }
    }

    if (covariates.length > 0) {
      for (c <- covariates) {
        val value = c.eval(series)
        if (value == 0.0)
          return 0.0
        res *= value
      }
    }

    res
  }
}

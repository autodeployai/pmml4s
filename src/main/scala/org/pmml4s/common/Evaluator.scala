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
import org.pmml4s.util.Utils

/**
 * A common super-trait that accepts a series, then evaluates a single value.
 */
trait Evaluator extends PmmlElement {
  def eval(series: Series): Any
}

trait DoubleEvaluator extends PmmlElement {
  def asDouble(series: Series): Double
}

trait RegressionEvaluator extends Evaluator with DoubleEvaluator {
  override def eval(series: Series): Double

  override def asDouble(series: Series): Double = eval(series)
}

trait MixedEvaluator extends Evaluator with DoubleEvaluator {
  override def asDouble(series: Series): Double = Utils.toDouble(eval(series))
}
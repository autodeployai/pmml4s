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

import org.pmml4s.util.Utils
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

/**
 * Test cases of built-in functions.
 */
class FunctionsTest extends FunSuite {
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.000001)

  test("modulo") {
    assert(Modulo.eval(11, 3) === 2.0)
    assert(Modulo.eval(-17.2, 0.5) === 0.3)
    assert(Modulo.eval(9, -7) === -5.0)
    assert(Modulo.eval(-4, -9) === -4.0)

    val f = BuiltInFunctions.getFunction("x-sin")
    assert(f.isDefined)
  }

  test("aggregation functions") {
    assert(Min.eval(1.0, 3.0, 2.0) === 1.0)
    assert(Min.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 1.0)
    assert(Utils.isMissing(Min.eval(Double.NaN, Double.NaN)))

    assert(Max.eval(1.0, 3.0, 2.0) === 3.0)
    assert(Max.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 3.0)
    assert(Utils.isMissing(Max.eval(Double.NaN, Double.NaN)))

    assert(Sum.eval(1.0, 3.0, 2.0) === 6.0)
    assert(Sum.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 6.0)
    assert(Utils.isMissing(Sum.eval(Double.NaN, Double.NaN)))

    assert(Avg.eval(1.0, 3.0, 2.0) === 2.0)
    assert(Avg.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 2.0)
    assert(Utils.isMissing(Avg.eval(Double.NaN, Double.NaN)))

    assert(Median.eval(1.0, 3.0, 2.0) === 2.0)
    assert(Median.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 2.0)
    assert(Median.eval(1.0, 3.0, 2.0, 4.0) === 2.5)
    assert(Median.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN, 4.0) === 2.5)
    assert(Utils.isMissing(Median.eval(Double.NaN, Double.NaN)))

    assert(Product.eval(1.0, 3.0, 2.0) === 6.0)
    assert(Product.eval(1.0, Double.NaN, 3.0, 2.0, Double.NaN) === 6.0)
    assert(Utils.isMissing(Product.eval(Double.NaN, Double.NaN)))
  }
}

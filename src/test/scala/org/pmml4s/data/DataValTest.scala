/*
 * Copyright (c) 2024 AutoDeployAI
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
package org.pmml4s.data

import org.scalatest.funsuite.AnyFunSuite

class DataValTest extends AnyFunSuite {

  test("DataVal") {
    assert(DataVal.`0.0` === DoubleVal(0.0))
    assert(DataVal.`1.0` === DoubleVal(1.0))
    assert(DataVal.`1` === LongVal(1L) )
    assert(DataVal.NaN.value != DataVal.NaN.value)
    assert(DataVal.NaN.isMissing)
    assert(DataVal.NULLString === StringVal(null))
    assert(DataVal.NULLString.isMissing)
    assert(DataVal.EmptyString === StringVal(""))
    assert(DataVal.NULLDate === DateVal(null))
    assert(DataVal.NULLDate.isMissing)
    assert(DataVal.NULL === NullVal)
    assert(DataVal.NULL.isMissing)
    assert(DataVal.TRUE.toBool)
    assert(!DataVal.FALSE.toBool)
  }

  test("DoubleVal") {
    val d0 = DoubleVal(0.0)
    val d1 = DoubleVal(1.0)
    val d2 = DoubleVal(2.0)

    assert(d0 === DoubleVal(0.0))
    assert(d1.equals(1.0))
    assert(d2 === DataVal.from(2.0))
  }

  test("FloatVal") {
    val f0 = FloatVal(0.0f)
    val f1 = FloatVal(1.0f)
    val f2 = FloatVal(2.0f)

    assert(f0 === FloatVal(0.0f))
    assert(f1.equals(1.0f))
    assert(f2 === DataVal.from(2.0f))
  }

  test("LongVal") {
    val l0 = LongVal(0)
    val l1 = LongVal(1)
    val l2 = LongVal(2)

    assert(l0 === LongVal(0))
    assert(l1.equals(1))
    assert(l2 === DataVal.from(2))
  }

}

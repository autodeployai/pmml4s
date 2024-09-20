/*
 * Copyright (c) 2023 AutoDeployAI
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
package org.pmml4s.util

import org.scalatest._
import funsuite._
import org.pmml4s.common.{BooleanType, DoubleType, IntegerType, RealType, StringType}
import org.pmml4s.data.DataVal

class UtilsTest extends AnyFunSuite {

  test("isMissing") {
    import Utils.isMissing

    assert(!isMissing(""))
    assert(!isMissing("string"))
    assert(!isMissing(0.0))
    assert(!isMissing(0))
    assert(!isMissing(0L))
    assert(!isMissing(true))
    assert(!isMissing(false))

    assert(isMissing(Double.NaN))
    assert(isMissing(null))

    val a: Any = Double.NaN
    assert(isMissing(a))
  }

  test("inferDataType") {
    import Utils.inferDataType

    assert(inferDataType(0.0) == RealType)
    assert(inferDataType(0) == IntegerType)
    assert(inferDataType("") == StringType)
    assert(inferDataType(false) == BooleanType)
    assert(inferDataType(true) == BooleanType)

    assert(inferDataType(DataVal.`0.0`) == RealType)
    assert(inferDataType(DataVal.`1.0`) == RealType)
    assert(inferDataType(DataVal.`1`) == IntegerType)
    assert(inferDataType(DataVal.NaN) == RealType)
    assert(inferDataType(DataVal.EmptyString) == StringType)
    assert(inferDataType(DataVal.NULLString) == StringType)
  }
}


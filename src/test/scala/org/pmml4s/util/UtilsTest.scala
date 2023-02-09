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
}


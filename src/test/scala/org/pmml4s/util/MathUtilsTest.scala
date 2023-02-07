/*
 * Copyright (c) 2017-2023 AutoDeployAI
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

/**
 * Test cases of math utilities.
 */
class MathUtilsTest extends AnyFunSuite {

  test("weightedMedian") {
    val a = MathUtils.weightedMedian(Array(1), Array(0.15))
    assert(a === 1)

    val b = MathUtils.weightedMedian(Array(1, 2, 3, 4, 5), Array(0.15, 0.1, 0.2, 0.3, 0.25))
    assert(b === 4)
  }

}


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
package org.pmml4s.data

import org.scalatest._
import funsuite._

class SeriesTest extends AnyFunSuite {

  test("filter") {
    val series = Series.fromMap(Map("f1" -> 1.0, "f2" -> 2.0, "f3" -> 3.0, "f4" -> 4.0))
    assert(series.filter(null).toArray  === Array(1.0, 2.0, 3.0, 4.0))
    assert(series.filter(Seq()).toArray  === Array(1.0, 2.0, 3.0, 4.0))

    val result1 = series.filter(Seq("f1", "f3"))
    assert(result1.toArray  === Array(1.0, 3.0))

    val result2 = series.filter(Seq("f4", "f2"))
    assert(result2.toArray  === Array(4.0, 2.0))

    val result3 = series.filter(Seq("f3", "f4", "f5"))
    assert(result3.toArray  === Array(3.0, 4.0))
  }
}



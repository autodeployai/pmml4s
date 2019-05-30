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
package org.pmml4s.data

import org.scalatest.FunSuite

class CombinedSeriesTest extends FunSuite {

  test("Basic") {
    val series1 = Series(0, 1, 2)
    val series2 = Series(3, 4, 5)
    val series3 = Series(6, 7, 8, 9)

    val empty = Series.merge()
    assert(empty.length === 0)

    val seriesA = Series.merge(series1)
    assert(seriesA.length === 3)
    assert(seriesA(0) === 0)
    assert(seriesA(1) === 1)
    assert(seriesA(2) === 2)
    assert(seriesA(3) === null)

    val seriesB = Series.merge(series1, series2)
    assert(seriesB.length === 6)
    assert(seriesB(0) === 0)
    assert(seriesB(1) === 1)
    assert(seriesB(2) === 2)
    assert(seriesB(3) === 3)
    assert(seriesB(4) === 4)
    assert(seriesB(5) === 5)
    assert(seriesB(6) === null)

    val seriesC = Series.merge(series1, series2, series3)
    assert(seriesC.length === 10)
    assert(seriesC(0) === 0)
    assert(seriesC(1) === 1)
    assert(seriesC(2) === 2)
    assert(seriesC(3) === 3)
    assert(seriesC(4) === 4)
    assert(seriesC(5) === 5)
    assert(seriesC(6) === 6)
    assert(seriesC(7) === 7)
    assert(seriesC(8) === 8)
    assert(seriesC(9) === 9)
    assert(seriesA(10) === null)
  }

}

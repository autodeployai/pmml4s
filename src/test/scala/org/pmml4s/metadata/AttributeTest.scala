/*
 * Copyright (c) 2022 AutoDeploy AI
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
package org.pmml4s.metadata

import org.pmml4s.common.Interval
import org.pmml4s.util.Utils
import org.scalatest.FunSuite

class AttributeTest extends FunSuite {

  test("Categorical attribute with predefined values") {
    val catAttr = CategoricalAttribute(
      values=Array("VALID1", "VALID2"),
      invalidVals=Set("INVALID1", "INVALID2"),
      missingVals=Set("MISSING1", "MISSING2"),
      labels=Map("VALID1" -> "LABEL1", "VALID2" -> "LABEL2")
    )

    // valid values
    assert(!catAttr.isValidValue(null))
    assert(!catAttr.isValidValue(Double.NaN))
    assert(catAttr.isValidValue("VALID1"))
    assert(catAttr.isValidValue("VALID2"))
    assert(!catAttr.isValidValue("INVALID1"))
    assert(!catAttr.isValidValue("INVALID2"))
    assert(!catAttr.isValidValue("MISSING1"))
    assert(!catAttr.isValidValue("MISSING2"))
    assert(!catAttr.isValidValue("UNKNOWN"))

    // missing values
    assert(catAttr.isMissingValue(null))
    assert(catAttr.isMissingValue(Double.NaN))
    assert(!catAttr.isMissingValue("VALID1"))
    assert(!catAttr.isMissingValue("VALID2"))
    assert(!catAttr.isMissingValue("INVALID1"))
    assert(!catAttr.isMissingValue("INVALID2"))
    assert(catAttr.isMissingValue("MISSING1"))
    assert(catAttr.isMissingValue("MISSING2"))
    assert(!catAttr.isMissingValue("UNKNOWN"))

    // invalid values
    assert(!catAttr.isInvalidValue(null))
    assert(!catAttr.isInvalidValue(Double.NaN))
    assert(!catAttr.isInvalidValue("VALID1"))
    assert(!catAttr.isInvalidValue("VALID2"))
    assert(catAttr.isInvalidValue("INVALID1"))
    assert(catAttr.isInvalidValue("INVALID2"))
    assert(!catAttr.isInvalidValue("MISSING1"))
    assert(!catAttr.isInvalidValue("MISSING2"))
    assert(catAttr.isInvalidValue("UNKNOWN"))

    // encode
    assert(Utils.isMissing(catAttr.encode(null)))
    assert(Utils.isMissing(catAttr.encode(Double.NaN)))
    assert(catAttr.encode("VALID1") === 0)
    assert(catAttr.encode("VALID2") === 1)
    assert(Utils.isMissing(catAttr.encode("INVALID1")))
    assert(Utils.isMissing(catAttr.encode("INVALID2")))
    assert(Utils.isMissing(catAttr.encode("MISSING1")))
    assert(Utils.isMissing(catAttr.encode("MISSING2")))
    assert(Utils.isMissing(catAttr.encode("UNKNOWN")))
  }

  test("Categorical attribute without predefined values") {
    val catAttr = CategoricalAttribute(
      values=Array.empty,
      invalidVals=Set("INVALID1", "INVALID2"),
      missingVals=Set("MISSING1", "MISSING2"),
      labels=Map.empty
    )

    // valid values
    assert(!catAttr.isValidValue(null))
    assert(!catAttr.isValidValue(Double.NaN))
    assert(!catAttr.isValidValue("INVALID1"))
    assert(!catAttr.isValidValue("INVALID2"))
    assert(!catAttr.isValidValue("MISSING1"))
    assert(!catAttr.isValidValue("MISSING2"))
    assert(catAttr.isValidValue("UNKNOWN"))

    // missing values
    assert(catAttr.isMissingValue(null))
    assert(catAttr.isMissingValue(Double.NaN))
    assert(!catAttr.isMissingValue("INVALID1"))
    assert(!catAttr.isMissingValue("INVALID2"))
    assert(catAttr.isMissingValue("MISSING1"))
    assert(catAttr.isMissingValue("MISSING2"))
    assert(!catAttr.isMissingValue("UNKNOWN"))

    // invalid values
    assert(!catAttr.isInvalidValue(null))
    assert(!catAttr.isInvalidValue(Double.NaN))
    assert(catAttr.isInvalidValue("INVALID1"))
    assert(catAttr.isInvalidValue("INVALID2"))
    assert(!catAttr.isInvalidValue("MISSING1"))
    assert(!catAttr.isInvalidValue("MISSING2"))
    assert(!catAttr.isInvalidValue("UNKNOWN"))

    // encode
    assert(Utils.isMissing(catAttr.encode(null)))
    assert(Utils.isMissing(catAttr.encode(Double.NaN)))
    assert(Utils.isMissing(catAttr.encode("INVALID1")))
    assert(Utils.isMissing(catAttr.encode("INVALID2")))
    assert(Utils.isMissing(catAttr.encode("MISSING1")))
    assert(Utils.isMissing(catAttr.encode("MISSING2")))
    assert(catAttr.encode("UNKNOWN1") === 0)
    assert(catAttr.encode("UNKNOWN2") === 1)
  }

  test("Immutable categorical attribute without predefined values") {
    val catAttr = new ImmutableCategoricalAttribute(
      validValues=Array.empty,
      invalidValues=Set("INVALID1", "INVALID2"),
      missingValues=Set("MISSING1", "MISSING2"),
      labels=Map("VALID1" -> "LABEL1", "VALID2" -> "LABEL2")
    )

    // valid values
    assert(!catAttr.isValidValue(null))
    assert(!catAttr.isValidValue(Double.NaN))
    assert(!catAttr.isValidValue("INVALID1"))
    assert(!catAttr.isValidValue("INVALID2"))
    assert(!catAttr.isValidValue("MISSING1"))
    assert(!catAttr.isValidValue("MISSING2"))
    assert(catAttr.isValidValue("UNKNOWN"))

    // missing values
    assert(catAttr.isMissingValue(null))
    assert(catAttr.isMissingValue(Double.NaN))
    assert(!catAttr.isMissingValue("INVALID1"))
    assert(!catAttr.isMissingValue("INVALID2"))
    assert(catAttr.isMissingValue("MISSING1"))
    assert(catAttr.isMissingValue("MISSING2"))
    assert(!catAttr.isMissingValue("UNKNOWN"))

    // invalid values
    assert(!catAttr.isInvalidValue(null))
    assert(!catAttr.isInvalidValue(Double.NaN))
    assert(catAttr.isInvalidValue("INVALID1"))
    assert(catAttr.isInvalidValue("INVALID2"))
    assert(!catAttr.isInvalidValue("MISSING1"))
    assert(!catAttr.isInvalidValue("MISSING2"))
    assert(!catAttr.isInvalidValue("UNKNOWN"))

    // encode
    assert(Utils.isMissing(catAttr.encode(null)))
    assert(Utils.isMissing(catAttr.encode(Double.NaN)))
    assert(Utils.isMissing(catAttr.encode("INVALID1")))
    assert(Utils.isMissing(catAttr.encode("INVALID2")))
    assert(Utils.isMissing(catAttr.encode("MISSING1")))
    assert(Utils.isMissing(catAttr.encode("MISSING2")))
    assert(catAttr.encode(0) === 0)
    assert(catAttr.encode(1) === 1)
    assert(Utils.isMissing(catAttr.encode("UNKNOWN")))
  }

  test("Continuous attribute with an interval and discrete missing values") {
    val contAttr = ContinuousAttribute(Interval.closed(0, 100), Set(-999, 999))

    // valid values
    assert(!contAttr.isValidValue(null))
    assert(!contAttr.isValidValue(Double.NaN))
    assert(contAttr.isValidValue(0))
    assert(contAttr.isValidValue(100))
    assert(contAttr.isValidValue(50))
    assert(!contAttr.isValidValue(-1))
    assert(!contAttr.isValidValue(101))
    assert(!contAttr.isValidValue(-999))
    assert(!contAttr.isValidValue(999))

    // missing values
    assert(contAttr.isMissingValue(null))
    assert(contAttr.isMissingValue(Double.NaN))
    assert(!contAttr.isMissingValue(0))
    assert(!contAttr.isMissingValue(100))
    assert(!contAttr.isMissingValue(50))
    assert(!contAttr.isMissingValue(-1))
    assert(!contAttr.isMissingValue(101))
    assert(contAttr.isMissingValue(-999))
    assert(contAttr.isMissingValue(999))

    // invalid values
    assert(!contAttr.isInvalidValue(null))
    assert(!contAttr.isInvalidValue(Double.NaN))
    assert(!contAttr.isInvalidValue(0))
    assert(!contAttr.isInvalidValue(100))
    assert(!contAttr.isInvalidValue(50))
    assert(contAttr.isInvalidValue(-1))
    assert(contAttr.isInvalidValue(101))
    assert(!contAttr.isInvalidValue(-999))
    assert(!contAttr.isInvalidValue(999))

    // encode
    assert(Utils.isMissing(contAttr.encode(null)))
    assert(Utils.isMissing(contAttr.encode(Double.NaN)))
    assert(contAttr.encode(0) === 0)
    assert(contAttr.encode(100) === 100)
    assert(contAttr.encode(50) === 50)
    assert(contAttr.encode(-1) === -1)
    assert(contAttr.encode(101) === 101)
    assert(Utils.isMissing(contAttr.encode(-999)))
    assert(Utils.isMissing(contAttr.encode(999)))
  }

  test("Continuous attribute with discrete valid values") {
    val contAttr = ContinuousAttribute(
      intervals = Array.empty,
      values = Array(1, 2, 3),
      invalidVals = Set.empty,
      missingVals = Set(-999, 999),
      labels= Map.empty
    )

    // valid values
    assert(!contAttr.isValidValue(null))
    assert(!contAttr.isValidValue(Double.NaN))
    assert(contAttr.isValidValue(1))
    assert(contAttr.isValidValue(2))
    assert(contAttr.isValidValue(3))
    assert(!contAttr.isValidValue(0))
    assert(!contAttr.isValidValue(4))
    assert(!contAttr.isValidValue(-999))
    assert(!contAttr.isValidValue(999))

    // missing values
    assert(contAttr.isMissingValue(null))
    assert(contAttr.isMissingValue(Double.NaN))
    assert(!contAttr.isMissingValue(1))
    assert(!contAttr.isMissingValue(2))
    assert(!contAttr.isMissingValue(3))
    assert(!contAttr.isMissingValue(0))
    assert(!contAttr.isMissingValue(4))
    assert(contAttr.isMissingValue(-999))
    assert(contAttr.isMissingValue(999))

    // invalid values
    assert(!contAttr.isInvalidValue(null))
    assert(!contAttr.isInvalidValue(Double.NaN))
    assert(!contAttr.isInvalidValue(1))
    assert(!contAttr.isInvalidValue(2))
    assert(!contAttr.isInvalidValue(3))
    assert(contAttr.isInvalidValue(0))
    assert(contAttr.isInvalidValue(4))
    assert(!contAttr.isInvalidValue(-999))
    assert(!contAttr.isInvalidValue(999))

    // encode
    assert(Utils.isMissing(contAttr.encode(null)))
    assert(Utils.isMissing(contAttr.encode(Double.NaN)))
    assert(contAttr.encode(1) === 1)
    assert(contAttr.encode(2) === 2)
    assert(contAttr.encode(3) === 3)
    assert(contAttr.encode(0) === 0)
    assert(contAttr.encode(4) === 4)
    assert(Utils.isMissing(contAttr.encode(-999)))
    assert(Utils.isMissing(contAttr.encode(999)))
  }

  test("Continuous attribute with discrete missing and invalid values") {
    val contAttr = ContinuousAttribute(
      intervals = Array.empty,
      values = Array.empty,
      invalidVals = Set(-100, 100),
      missingVals = Set(-999, 999),
      labels= Map.empty
    )

    // valid values
    assert(!contAttr.isValidValue(null))
    assert(!contAttr.isValidValue(Double.NaN))
    assert(contAttr.isValidValue(1))
    assert(contAttr.isValidValue(2))
    assert(contAttr.isValidValue(3))
    assert(!contAttr.isValidValue(-100))
    assert(!contAttr.isValidValue(100))
    assert(!contAttr.isValidValue(-999))
    assert(!contAttr.isValidValue(999))

    // missing values
    assert(contAttr.isMissingValue(null))
    assert(contAttr.isMissingValue(Double.NaN))
    assert(!contAttr.isMissingValue(1))
    assert(!contAttr.isMissingValue(2))
    assert(!contAttr.isMissingValue(3))
    assert(!contAttr.isMissingValue(-100))
    assert(!contAttr.isMissingValue(100))
    assert(contAttr.isMissingValue(-999))
    assert(contAttr.isMissingValue(999))

    // invalid values
    assert(!contAttr.isInvalidValue(null))
    assert(!contAttr.isInvalidValue(Double.NaN))
    assert(!contAttr.isInvalidValue(1))
    assert(!contAttr.isInvalidValue(2))
    assert(!contAttr.isInvalidValue(3))
    assert(contAttr.isInvalidValue(-100))
    assert(contAttr.isInvalidValue(100))
    assert(!contAttr.isInvalidValue(-999))
    assert(!contAttr.isInvalidValue(999))

    // encode
    assert(Utils.isMissing(contAttr.encode(null)))
    assert(Utils.isMissing(contAttr.encode(Double.NaN)))
    assert(contAttr.encode(1) === 1)
    assert(contAttr.encode(2) === 2)
    assert(contAttr.encode(3) === 3)
    assert(contAttr.encode(-100) === -100)
    assert(contAttr.encode(100) === 100)
    assert(Utils.isMissing(contAttr.encode(-999)))
    assert(Utils.isMissing(contAttr.encode(999)))
  }
}


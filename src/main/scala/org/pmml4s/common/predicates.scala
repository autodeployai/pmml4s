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

import org.pmml4s.common.Operator.Operator
import org.pmml4s.data.Series
import org.pmml4s.metadata.Field
import org.pmml4s.util.Utils
import org.pmml4s.xml.ElemTags

object Predication extends Enumeration {
  type Predication = Value
  val TRUE, FALSE, UNKNOWN, SURROGATE = Value

  def fire(r: Predication): Boolean = r == TRUE || r == SURROGATE
}

import org.pmml4s.common.Predication._

trait Predicate extends PmmlElement {

  /** Evaluates the predicate. */
  def eval(series: Series): Predication
}

object Predicate {

  import ElemTags._

  val values = Set(SIMPLE_PREDICATE, COMPOUND_PREDICATE, SIMPLE_SET_PREDICATE, TRUE, FALSE)

  def contains(s: String) = values.contains(s)
}

/** Pre-defined comparison operators. */
object Operator extends Enumeration {
  type Operator = Value
  val equal, notEqual, lessThan, lessOrEqual, greaterThan, greaterOrEqual, isMissing, isNotMissing = Value
}

object SimpleSetPredicate {

  /**
   *  - isIn indicates an evaluation to TRUE if the field value is contained in the list of values in the array.
   *  - isNotIn indicates an evaluation to TRUE if the field value is not contained in the list of values in the array.
   */
  object BooleanOperator extends Enumeration {
    type BooleanOperator = Value
    val isIn, isNotIn = Value
  }

}

object CompoundPredicate {

  /**
   *  - and indicates an evaluation to TRUE if all the predicates evaluate to TRUE.
   *  - or indicates an evaluation to TRUE if one of the predicates evaluates to TRUE.
   *  - xor indicates an evaluation to TRUE if an odd number of the predicates evaluates to TRUE and all others evaluate
   * to FALSE.
   *  - surrogate allows for specifying surrogate predicates. They are used for cases where a missing value appears in
   * the evaluation of the parent predicate such that an alternative predicate is available.
   */
  object BooleanOperator extends Enumeration {
    type BooleanOperator = Value
    val or, and, xor, surrogate = Value
  }

}

import org.pmml4s.common.Operator._

/**
 * Defines a rule in the form of a simple boolean expression. The rule consists of field, operator (booleanOperator) for binary
 * comparison, and value.
 */
class SimplePredicate(
                       val field: Field,
                       val operator: Operator,
                       val value: Double = Double.NaN) extends Predicate {

  def eval(input: Series): Predication = {
    val v = field.encode(input)
    operator match {
      case `equal`          => if (Utils.isMissing(v)) UNKNOWN else if (v == value) TRUE else FALSE
      case `notEqual`       => if (Utils.isMissing(v)) UNKNOWN else if (v != value) TRUE else FALSE
      case `lessThan`       => if (Utils.isMissing(v)) UNKNOWN else if (v < value) TRUE else FALSE
      case `lessOrEqual`    => if (Utils.isMissing(v)) UNKNOWN else if (v <= value) TRUE else FALSE
      case `greaterThan`    => if (Utils.isMissing(v)) UNKNOWN else if (v > value) TRUE else FALSE
      case `greaterOrEqual` => if (Utils.isMissing(v)) UNKNOWN else if (v >= value) TRUE else FALSE
      case `isMissing`      => if (Utils.isMissing(v)) TRUE else FALSE
      case `isNotMissing`   => if (!Utils.isMissing(v)) TRUE else FALSE
    }
  }
}

/**
 * CompoundPredicate: an encapsulating element for combining two or more elements as defined at the entity PREDICATE.
 * The attribute associated with this element, booleanOperator, can take one of the following logical (boolean) operators:
 * and, or, xor or surrogate.
 */
class CompoundPredicate(
                         val booleanOperator: CompoundPredicate.BooleanOperator.Value,
                         val children: Array[Predicate]) extends Predicate {

  import CompoundPredicate.BooleanOperator._

  def eval(input: Series): Predication = booleanOperator match {
    case `or`        => {
      var hasUnknown = false
      for (child <- children) {
        val r = child.eval(input)
        if (r == TRUE)
          return TRUE
        else if (r == UNKNOWN)
          hasUnknown = true
      }

      if (hasUnknown) UNKNOWN else FALSE
    }
    case `and`       => {
      var hasUnknown = false
      for (child <- children) {
        val r = child.eval(input)
        if (r == FALSE)
          return FALSE
        else if (r == UNKNOWN)
          hasUnknown = true
      }

      if (hasUnknown) UNKNOWN else TRUE
    }
    case `xor`       => {
      var count = 0
      for (child <- children) {
        val r = child.eval(input)
        if (r == UNKNOWN)
          return UNKNOWN
        else if (r == TRUE)
          count += 1
      }

      if (count % 2 == 1) TRUE else FALSE
    }
    case `surrogate` => {
      var isSurrogate = false
      for (child <- children) {
        val r = child.eval(input)
        if (r != UNKNOWN)
          return if (r == TRUE) {
            if (isSurrogate) SURROGATE else TRUE
          } else r
        else
          isSurrogate = true
      }

      UNKNOWN
    }
  }
}

/**
 * Checks whether a field value is element of a set.
 * The set of values is specified by the array.
 */
class SimpleSetPredicate(
                          val field: Field,
                          val booleanOperator: SimpleSetPredicate.BooleanOperator.Value,
                          val values: Set[Double]) extends Predicate {

  import SimpleSetPredicate.BooleanOperator._

  def eval(input: Series): Predication = {
    val v = field.encode(input)
    booleanOperator match {
      case `isIn`    => if (Utils.isMissing(v)) UNKNOWN else if (values.contains(v)) TRUE else FALSE
      case `isNotIn` => if (Utils.isMissing(v)) UNKNOWN else if (!values.contains(v)) TRUE else FALSE
    }
  }
}

/** Identifies the boolean constant TRUE. */
object True extends Predicate {
  def eval(input: Series): Predication = TRUE
}

/** Identifies the boolean constant FALSE. */
object False extends Predicate {
  def eval(input: Series): Predication = FALSE
}


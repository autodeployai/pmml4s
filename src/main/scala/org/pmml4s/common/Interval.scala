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
package org.pmml4s.common

object Closure extends Enumeration {
  type Closure = Value
  val openClosed, openOpen, closedOpen, closedClosed = Value

  def isClosedBelow(closure: Closure): Boolean = closure == closedOpen || closure == closedClosed

  def isClosedAbove(closure: Closure): Boolean = closure == openClosed || closure == closedClosed

  def isOpenBelow(closure: Closure): Boolean = closure == openClosed || closure == openOpen

  def isOpenAbove(closure: Closure): Boolean = closure == closedOpen || closure == openOpen
}

/**
 * Defines a range of numeric values.
 */
sealed abstract class Interval extends PmmlElement {
  def contains(value: Double): Boolean = !value.isNaN
}

import org.pmml4s.common.Closure.Closure
import org.pmml4s.util.Utils

case class GenericInterval(val leftMargin: Double, val rightMargin: Double, val closure: Closure) extends Interval {

  override def contains(value: Double): Boolean = super.contains(value) && {
    closure match {
      case Closure.openClosed   => value > leftMargin && value <= rightMargin
      case Closure.openOpen     => value > leftMargin && value < rightMargin
      case Closure.closedOpen   => value >= leftMargin && value < rightMargin
      case Closure.closedClosed => value >= leftMargin && value <= rightMargin
    }
  }

  override def toString: String = closure match {
    case Closure.openClosed   => s"(${leftMargin},${rightMargin}]"
    case Closure.openOpen     => s"(${leftMargin},${rightMargin})"
    case Closure.closedOpen   => s"[${leftMargin},${rightMargin})"
    case Closure.closedClosed => s"[${leftMargin},${rightMargin}]"
  }
}

case class BelowInterval(val rightMargin: Double, val isOpen: Boolean) extends Interval {

  override def contains(value: Double): Boolean = super.contains(value) && {
    if (isOpen) value < rightMargin else value <= rightMargin
  }

  override def toString: String = s"(,${rightMargin}${if (isOpen) ")" else "]"}"
}

case class AboveInterval(val leftMargin: Double, val isOpen: Boolean) extends Interval {

  override def contains(value: Double): Boolean = super.contains(value) && {
    if (isOpen) value > leftMargin else value >= leftMargin
  }

  override def toString: String = s"(${leftMargin}${if (isOpen) "(" else "["},)"
}

case class PointInterval(val x: Double) extends Interval {
  override def contains(value: Double): Boolean = super.contains(value) && value == x

  override def toString: String = s"[${x},${x}]"
}

case object InfinityInterval extends Interval {
  override def contains(value: Double): Boolean = super.contains(value)

  override def toString: String = ""
}

object Interval {

  def apply(): Interval = InfinityInterval

  def apply(leftMargin: Double, rightMargin: Double, closure: Closure): Interval = GenericInterval(leftMargin, rightMargin, closure)

  def above(leftMargin: Double): Interval = AboveInterval(leftMargin, true)

  def atOrAbove(leftMargin: Double): Interval = AboveInterval(leftMargin, false)

  def below(rightMargin: Double) = BelowInterval(rightMargin, true)

  def atOrBelow(rightMargin: Double) = BelowInterval(rightMargin, false)

  def closed(leftMargin: Double, rightMargin: Double): Interval = GenericInterval(leftMargin, rightMargin, Closure.closedClosed)

  def open(leftMargin: Double, rightMargin: Double): Interval = GenericInterval(leftMargin, rightMargin, Closure.openOpen)

  def openClosed(leftMargin: Double, rightMargin: Double): Interval = GenericInterval(leftMargin, rightMargin, Closure.openClosed)

  def closedOpen(leftMargin: Double, rightMargin: Double): Interval = GenericInterval(leftMargin, rightMargin, Closure.closedOpen)
}

trait HasIntervals {
  def intervals: Array[Interval]

  def isIn(value: Any): Boolean = if (intervals.length == 0) true else intervals.exists(x => x.contains(Utils.toDouble(value)))
}


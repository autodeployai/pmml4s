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

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, GregorianCalendar, TimeZone}

import org.apache.commons.math3.distribution.NormalDistribution
import org.pmml4s.FunctionNotFoundException
import org.pmml4s.common.PmmlElement
import org.pmml4s.data.Datetime
import org.pmml4s.util.Utils

import scala.collection.mutable


trait FunctionProvider {
  def function(name: String): Function = getFunction(name).getOrElse(throw new FunctionNotFoundException(name))

  def getFunction(name: String): Option[Function]
}

object BuiltInFunctions extends FunctionProvider {
  lazy val functions: Map[String, Function] = {
    val set = Set(Add, Subtract, Multiply, Divide, // Functions for simple arithmetics.
      Min, Max, Sum, Avg, Median, Product, // Returns an aggregation of a variable number of input fields.
      Log10, Ln, Sqrt, Abs, Exp, Pow, Threshold, Floor, Ceil, Round, Modulo, // Further mathematical functions.
      IsMissing, IsNotMissing, IsValid, IsNotValid, // Functions for boolean operations.
      Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual, // Further boolean functions.
      And, Or, Not, IsIn, IsNotIn, If, // Further boolean functions.
      Uppercase, Lowercase, StringLength, Substring, TrimBlanks, Concat, Replace, Matches, FormatNumber, // Functions for string operations.
      FormatDatetime, DateDaysSinceYear, DateSecondsSinceYear, DateSecondsSinceMidnight, // Function for transforming dates into integers.
      NormalCDF, NormalPDF, StdNormalCDF, StdNormalPDF, Erf, NormalIDF, StdNormalIDF, // Functions for normal distribution
      Sin, ASin, SinH, Cos, ACos, CosH, Tan, ATan, TanH, // Trigonometric functions.
      Expm1, Hypot, Ln1p, RInt
    )

    set.map(x => (x.symbol, x)).toMap ++ set.filter(_.xSymbol.isDefined).map(x => (x.xSymbol.get, x)).toMap
  }

  def getFunction(symbol: String): Option[Function] = functions.get(symbol) orElse UserDefinedFunctions.getFunction(symbol)
}

class MutableFunctionProvider extends FunctionProvider {
  private val nameToFunction: mutable.Map[String, DefineFunction] = new mutable.HashMap[String, DefineFunction]()

  override def getFunction(name: String): Option[Function] = nameToFunction.get(name)

  def +=(f: DefineFunction): DefineFunction = {
    nameToFunction += f.name -> f
    f
  }
}

trait HasFunctionProvider {
  def provider: FunctionProvider
}

/**
 *
 */
trait Function extends PmmlElement {

  def apply(parameters: Any*): Any

  def symbol: String

  def xSymbol: Option[String] = None

  override def toString: String = symbol
}

trait UnaryFunction extends Function {

  override def apply(parameters: Any*): Any = {
    require(parameters.length == 1,
      s"Unary function ${symbol} should accept exactly one parameter, got ${parameters.length} parameters")
    eval(parameters(0))
  }

  def eval(a: Any): Any
}

trait BinaryFunction extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length == 2,
      s"Binary function ${symbol} should accept exactly two parameters, got ${parameters.length} parameters")
    eval(parameters(0), parameters(1))
  }

  def eval(left: Any, right: Any): Any
}

trait TernaryFunction extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length == 3,
      s"Ternary function ${symbol} should accept exactly three parameters, got ${parameters.length} parameters")
    eval(parameters(0), parameters(1), parameters(2))
  }

  def eval(a: Any, b: Any, c: Any): Any
}

trait UnaryArithmetic extends UnaryFunction {
  override def eval(a: Any): Any = {
    eval(Utils.toDouble(a))
  }

  def eval(a: Double): Double
}

trait UnaryBoolean extends UnaryFunction {
  override def eval(a: Any): Boolean
}

trait UnaryString extends UnaryFunction {
  override def eval(a: Any): Any = {
    eval(Utils.toString(a))
  }

  def eval(a: String): String
}

trait BinaryArithmetic extends BinaryFunction {
  override def eval(left: Any, right: Any): Any = {
    eval(Utils.toDouble(left), Utils.toDouble(right))
  }

  def eval(left: Double, right: Double): Double
}

trait BinaryBoolean extends BinaryFunction {
  override def eval(left: Any, right: Any): Boolean
}

trait BinaryString extends BinaryFunction {
  override def eval(left: Any, right: Any): String
}

trait MultipleBoolean extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length >= 2,
      s"function ${symbol} should accept at least two parameter, got ${parameters.length} parameter")
    eval(parameters.map(x => Utils.toBoolean(x)): _*)
  }

  def eval(parameters: Boolean*): Boolean
}

trait BinaryCompare extends BinaryFunction {
  override def eval(left: Any, right: Any): Any = {
    eval(Utils.toDouble(left), Utils.toDouble(right))
  }

  def eval(left: Double, right: Double): Boolean
}

trait MultipleArithmetic extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length > 0, s"function ${symbol} should accept at least one parameter, got 0 parameter")
    eval(parameters.map(x => Utils.toDouble(x)): _*)
  }

  def eval(parameters: Double*): Double
}

trait TernaryArithmetic extends TernaryFunction {
  override def eval(a: Any, b: Any, c: Any): Any = {
    eval(Utils.toDouble(a), Utils.toDouble(b), Utils.toDouble(c))
  }

  def eval(a: Double, b: Double, c: Double): Double
}

object Add extends BinaryArithmetic {

  override def eval(left: Double, right: Double): Double = left + right

  override def symbol: String = "+"
}

object Subtract extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = left - right

  override def symbol: String = "-"
}

object Multiply extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = left * right

  override def symbol: String = "*"
}

object Divide extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = left / right

  override def symbol: String = "/"
}

object Min extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = parameters.min

  override def symbol: String = "min"
}

object Max extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = parameters.max

  override def symbol: String = "max"
}

object Sum extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = parameters.sum

  override def symbol: String = "sum"
}

object Avg extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = parameters.sum / parameters.length

  override def symbol: String = "avg"
}

object Median extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = {
    val ordered = parameters.sorted
    if (parameters.length % 2 == 0) {
      val i = parameters.length / 2
      (ordered(i - 1) + ordered(i)) / 2
    } else {
      ordered(parameters.length / 2)
    }
  }

  override def symbol: String = "median"
}

object Product extends MultipleArithmetic {
  override def eval(parameters: Double*): Double = {
    parameters.fold(1.0)((x1, x2) => x1 * x2)
  }

  override def symbol: String = "product"
}

object Log10 extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.log10(a)

  override def symbol: String = "log10"
}

object Ln extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.log(a)

  override def symbol: String = "ln"
}

object Sqrt extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.sqrt(a)

  override def symbol: String = "sqrt"
}

object Abs extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.abs(a)

  override def symbol: String = "abs"
}

object Exp extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.exp(a)

  override def symbol: String = "exp"
}

object Pow extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = Math.pow(left, right)

  override def symbol: String = "pow"
}

object Threshold extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = if (left > right) 1.0 else 0.0

  override def symbol: String = "threshold"
}

object Floor extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.floor(a)

  override def symbol: String = "floor"
}

object Ceil extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.ceil(a)

  override def symbol: String = "ceil"
}

object Round extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.round(a)

  override def symbol: String = "round"
}

object Modulo extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = left - Math.floor(left / right) * right

  override def symbol: String = "modulo"

  override def xSymbol: Option[String] = Some("x-modulo")
}

object IsMissing extends UnaryBoolean {
  override def eval(a: Any): Boolean = Utils.isMissing(a)

  override def symbol: String = "isMissing"
}

object IsNotMissing extends UnaryBoolean {
  override def eval(a: Any): Boolean = Utils.nonMissing(a)

  override def symbol: String = "isNotMissing"
}

object IsValid extends UnaryBoolean {
  override def eval(a: Any): Boolean = Utils.nonMissing(a)

  override def symbol: String = "isValid"
}

object IsNotValid extends UnaryBoolean {
  override def eval(a: Any): Boolean = Utils.isMissing(a)

  override def symbol: String = "isNotValid"
}

object Equal extends BinaryBoolean {
  override def eval(left: Any, right: Any): Boolean = left == right

  override def symbol: String = "equal"
}

object NotEqual extends BinaryBoolean {
  override def eval(left: Any, right: Any): Boolean = left != right

  override def symbol: String = "notEqual"
}

object LessThan extends BinaryCompare {
  override def eval(left: Double, right: Double): Boolean = left < right

  override def symbol: String = "lessThan"
}

object LessOrEqual extends BinaryCompare {
  override def eval(left: Double, right: Double): Boolean = left <= right

  override def symbol: String = "lessOrEqual"
}

object GreaterThan extends BinaryCompare {
  override def eval(left: Double, right: Double): Boolean = left > right

  override def symbol: String = "greaterThan"
}

object GreaterOrEqual extends BinaryCompare {
  override def eval(left: Double, right: Double): Boolean = left >= right

  override def symbol: String = "greaterOrEqual"
}

object And extends MultipleBoolean {
  override def eval(parameters: Boolean*): Boolean = {
    for (p <- parameters) {
      if (!p) return false
    }
    true
  }

  override def symbol: String = "and"
}

object Or extends MultipleBoolean {
  override def eval(parameters: Boolean*): Boolean = {
    for (p <- parameters) {
      if (p) return true
    }
    false
  }

  override def symbol: String = "or"
}

object Not extends UnaryFunction {
  override def eval(a: Any): Any = !Utils.toBoolean(a)

  override def symbol: String = "not"
}

object IsIn extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length >= 2, s"isIn should accept at least two parameters, got ${parameters.length}")
    parameters.tail.contains(parameters(0))
  }

  override def symbol: String = "isIn"
}


object IsNotIn extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length >= 2, s"isNotIn should accept at least two parameters, got ${parameters.length}")
    !parameters.tail.contains(parameters(0))
  }

  override def symbol: String = "isNotIn"
}

object If extends Function {
  override def apply(parameters: Any*): Any = {
    require(parameters.length == 2 || parameters.length == 3,
      s"If should accept two or three parameters, got ${parameters.length}")
    val cond = Utils.toBoolean(parameters(0))
    if (cond) parameters(1) else {
      if (parameters.length == 3) parameters(2) else null
    }
  }

  override def symbol: String = "if"
}

object Uppercase extends UnaryString {
  override def eval(a: String): String = if (a != null) a.toUpperCase else null

  override def symbol: String = "uppercase"
}

object Lowercase extends UnaryString {
  override def eval(a: String): String = if (a != null) a.toLowerCase else null

  override def symbol: String = "lowercase"
}

object TrimBlanks extends UnaryString {
  override def eval(a: String): String = if (a != null) a.trim else null

  override def symbol: String = "trimBlanks"
}

object Concat extends Function {
  override def apply(parameters: Any*): String = parameters.map({
    case null => ""
    case s    => s.toString
  }).mkString

  override def symbol: String = "concat"
}

object StringLength extends UnaryFunction {
  override def eval(a: Any): Any = a match {
    case s: String =>  s.length
    case _ => null
  }

  override def symbol: String = "stringLength"
}

object Substring extends TernaryFunction {
  override def eval(a: Any, b: Any, c: Any): String = {
    val s = Utils.toString(a)
    val pos = Utils.toInt(b)
    val length = Utils.toInt(c)

    if (s == null) {
      null
    } else if (pos <= 0 || length <= 0) {
      s
    } else {
      s.substring(pos - 1, Math.min(s.length, pos + length - 1))
    }
  }

  override def symbol: String = "substring"
}

object Replace extends TernaryFunction {
  override def eval(a: Any, b: Any, c: Any): Any = {
    val input = Utils.toString(a)
    val pattern = Utils.toString(b)
    val replacement = Utils.toString(c)

    if (input == null) {
      null
    } else if (pattern == null || replacement == null) {
      input
    } else {
      input.replaceAll(pattern, replacement)
    }
  }

  override def symbol: String = "replace"
}

object Matches extends BinaryBoolean {
  override def eval(left: Any, right: Any): Boolean = {
    val input = Utils.toString(left)
    val pattern = Utils.toString(right)

    if (input == null && pattern == null) {
      true
    } else if (input == null || pattern == null) {
      false
    } else {
      pattern.r.findFirstIn(input).isDefined
    }
  }

  override def symbol: String = "matches"
}

object FormatNumber extends BinaryFunction {
  override def eval(left: Any, right: Any): String = {
    val pattern = Utils.toString(right)
    if (pattern == null || left == null) {
      null
    } else {
      pattern.format(left)
    }
  }

  override def symbol: String = "formatNumber"
}

object FormatDatetime extends BinaryFunction {
  override def eval(left: Any, right: Any): String = {
    if (left == null || right == null)
      return null

    val date = left.asInstanceOf[Date]
    val pattern = Utils.toString(right)
    val format = new SimpleDateFormat(pattern)
    format.format(date)
  }

  override def symbol: String = "formatDatetime"
}

object DateDaysSinceYear extends BinaryFunction {
  override def eval(left: Any, right: Any): Any = {
    if (left == null || right == null)
      return null

    val date = left.asInstanceOf[Date]
    val refYear = Utils.toInt(right)

    val refDate = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    refDate.set(refYear, 0, 1)
    (date.getTime - refDate.getTimeInMillis) / Datetime.MILLISECONDS_PER_DAY
  }

  override def symbol: String = "dateDaysSinceYear"
}

object DateSecondsSinceYear extends BinaryFunction {
  override def eval(left: Any, right: Any): Any = {
    if (left == null || right == null)
      return null

    val date = left.asInstanceOf[Date]
    val refYear = Utils.toInt(right)

    val refDate = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    refDate.set(refYear, 0, 1)
    (date.getTime - refDate.getTimeInMillis) / 1000
  }

  override def symbol: String = "dateSecondsSinceYear"
}

object DateSecondsSinceMidnight extends UnaryFunction {
  override def eval(a: Any): Any = {
    if (a == null)
      return null

    val date = a.asInstanceOf[Date]
    val c = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    c.setTime(date)
    (c.get(Calendar.HOUR_OF_DAY) * 3600 + c.get(Calendar.MINUTE) * 60 + c.get(Calendar.SECOND))
  }

  override def symbol: String = "dateSecondsSinceMidnight"
}

object NormalCDF extends TernaryArithmetic {
  override def eval(a: Double, b: Double, c: Double): Double = {
    val dist = new NormalDistribution(b, c)
    dist.cumulativeProbability(a)
  }

  override def symbol: String = "normalCDF"
}

object StdNormalCDF extends UnaryArithmetic {
  override def eval(a: Double): Double = {
    val dist = new NormalDistribution
    dist.cumulativeProbability(a)
  }

  override def symbol: String = "stdNormalCDF"
}

object NormalPDF extends TernaryArithmetic {
  override def eval(a: Double, b: Double, c: Double): Double = {
    val dist = new NormalDistribution(b, c)
    dist.density(a)
  }

  override def symbol: String = "normalPDF"
}

object StdNormalPDF extends UnaryArithmetic {
  override def eval(a: Double): Double = {
    val dist = new NormalDistribution
    dist.density(a)
  }

  override def symbol: String = "stdNormalPDF"
}

object NormalIDF extends TernaryArithmetic {
  override def eval(a: Double, b: Double, c: Double): Double = {
    val dist = new NormalDistribution(b, c)
    dist.inverseCumulativeProbability(a)
  }

  override def symbol: String = "normalIDF"
}

object StdNormalIDF extends UnaryArithmetic {
  override def eval(a: Double): Double = {
    val dist = new NormalDistribution
    dist.inverseCumulativeProbability(a)
  }

  override def symbol: String = "stdNormalIDF"
}

object Erf extends UnaryArithmetic {
  override def eval(a: Double): Double = org.apache.commons.math3.special.Erf.erf(a)

  override def symbol: String = "erf"
}

object Sin extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.sin(a)

  override def symbol: String = "sin"

  override def xSymbol: Option[String] = Some("x-sin")
}

object ASin extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.asin(a)

  override def symbol: String = "asin"

  override def xSymbol: Option[String] = Some("x-asin")
}

object SinH extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.sinh(a)

  override def symbol: String = "sinh"

  override def xSymbol: Option[String] = Some("x-sinh")
}

object Cos extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.cos(a)

  override def symbol: String = "cos"

  override def xSymbol: Option[String] = Some("x-cos")
}

object ACos extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.acos(a)

  override def symbol: String = "acos"

  override def xSymbol: Option[String] = Some("x-acos")
}

object CosH extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.cosh(a)

  override def symbol: String = "cosh"

  override def xSymbol: Option[String] = Some("x-cosh")
}

object Tan extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.tan(a)

  override def symbol: String = "tan"

  override def xSymbol: Option[String] = Some("x-tan")
}

object ATan extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.atan(a)

  override def symbol: String = "atan"

  override def xSymbol: Option[String] = Some("x-atan")
}

object TanH extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.tanh(a)

  override def symbol: String = "tanh"

  override def xSymbol: Option[String] = Some("x-tanh")
}

object Expm1 extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.expm1(a)

  override def symbol: String = "expm1"

  override def xSymbol: Option[String] = Some("x-expm1")
}

object Hypot extends BinaryArithmetic {
  override def eval(left: Double, right: Double): Double = Math.hypot(left, right)

  override def symbol: String = "hypot"

  override def xSymbol: Option[String] = Some("x-hypot")
}

object Ln1p extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.log1p(a)

  override def symbol: String = "ln1p"

  override def xSymbol: Option[String] = Some("x-ln1p")
}

object RInt extends UnaryArithmetic {
  override def eval(a: Double): Double = Math.rint(a)

  override def symbol: String = "rint"

  override def xSymbol: Option[String] = Some("x-rint")
}

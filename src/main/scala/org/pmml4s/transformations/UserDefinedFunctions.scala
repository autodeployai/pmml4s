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

/**
 * Defines several user-defined functions produced by various vendors, actually, well-defined "DefineFunction" is fully
 * supported by pmml4s, while some could be not. Here is the place for those user-defined functions are not well defined.
 */
object UserDefinedFunctions extends FunctionProvider {
  lazy val functions: Map[String, Function] = {
    val set = Set(`SAS-EM-String-Normalize`, `SAS-FORMAT-BESTw`, `SAS-FORMAT-$CHARw`)
    set.map(x => (x.symbol, x)).toMap
  }

  def getFunction(symbol: String): Option[Function] = functions.get(symbol)
}

/**
 *
 * {{{
 * <DefineFunction name="SAS-EM-String-Normalize" optype="categorical" dataType="string">
 *  <ParameterField name="FMTWIDTH" optype="continuous"/>
 *  <ParameterField name="AnyCInput" optype="categorical"/>
 *  <Apply function="trimBlanks">
 *    <Apply function="uppercase">
 *      <Apply function="substring">
 *      <FieldRef field="AnyCInput"/>
 *      <Constant>1</Constant>
 *      <Constant>FMTWIDTH</Constant>
 *      </Apply>
 *    </Apply>
 *  </Apply>
 * </DefineFunction>
 * }}}
 */
object `SAS-EM-String-Normalize` extends BinaryFunction {
  override def eval(left: Any, right: Any): Any = {
    TrimBlanks.eval(Uppercase.eval(Substring.eval(right, 1, left)))
  }

  override def symbol: String = "SAS-EM-String-Normalize"
}

/**
 *
 * {{{
 * <DefineFunction name="SAS-FORMAT-BESTw" optype="categorical" dataType="string">
 *  <ParameterField name="FMTWIDTH" optype="continuous"/>
 *  <ParameterField name="AnyNInput" optype="continuous"/>
 *  <Apply function="formatNumber">
 *    <FieldRef field="AnyNInput"/>
 *    <Constant>FMTWIDTH</Constant>
 *  </Apply>
 * </DefineFunction>
 * }}}
 */
object `SAS-FORMAT-BESTw` extends BinaryFunction {
  override def eval(left: Any, right: Any): String = {
    FormatNumber.eval(left, right)
  }

  override def symbol: String = "SAS-FORMAT-BESTw"
}

/**
 *
 * {{{
 * <DefineFunction name="SAS-FORMAT-$CHARw" optype="categorical" dataType="string">
 *  <ParameterField name="FMTWIDTH" optype="continuous"/>
 *  <ParameterField name="AnyCInput" optype="continuous"/>
 *  <Apply function="substring">
 *    <FieldRef field="AnyCInput"/>
 *    <Constant>1</Constant>
 *    <Constant>FMTWIDTH</Constant>
 *  </Apply>
 * </DefineFunction>
 * }}}
 */
object `SAS-FORMAT-$CHARw` extends BinaryFunction {
  override def eval(left: Any, right: Any): String = {
    Substring.eval(right, 1, left)
  }

  override def symbol: String = "SAS-FORMAT-$CHARw"
}

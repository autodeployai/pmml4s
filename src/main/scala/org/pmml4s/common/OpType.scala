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

import org.pmml4s.xml.ValTags.{CATEGORICAL, CONTINUOUS, ORDINAL}

/**
 * Indicates which operations are defined on the values.
 */
sealed trait OpType

object OpType {

  /** Nominal field values can only be tested for equality. */
  case object nominal extends OpType

  /** Ordinal field values in addition have an order defined. */
  case object ordinal extends OpType

  /** Values of continuous fields can be used with arithmetic operators. */
  case object continuous extends OpType

  /** Not above three types. */
  case object typeless extends OpType

  def withName(s: String): OpType = s match {
    case CATEGORICAL => nominal
    case ORDINAL     => ordinal
    case CONTINUOUS  => continuous
    case _           => typeless
  }

  def withDataType(dateType: DataType): OpType = dateType match {
    case _: NumericType                 => continuous
    case _: StringType | _: BooleanType => nominal
    case _                              => typeless
  }

  /**
   * Tests whether this optype is categorical (nominal or ordinal).
   */
  def isCategorical(opType: OpType): Boolean = opType == OpType.nominal || opType == OpType.ordinal

  /**
   * Tests whether this optype is regression (continuous).
   */
  def isRegression(opType: OpType): Boolean = opType == OpType.continuous

  def isOrdinal(opType: OpType): Boolean = opType == OpType.ordinal
}

trait HasOpType {
  /** Operational type. */
  def opType: OpType

  /**
   * Tests whether this field is nominal.
   */
  def isNominal: Boolean = opType == OpType.nominal

  /**
   * Tests whether this field is ordinal.
   */
  def isOrdinal: Boolean = opType == OpType.ordinal

  /**
   * Tests whether this field is continuous.
   */
  def isContinuous: Boolean = opType == OpType.continuous

  /**
   * Tests whether this field is categorical (nominal or ordinal).
   */
  def isCategorical: Boolean = isNominal || isOrdinal
}
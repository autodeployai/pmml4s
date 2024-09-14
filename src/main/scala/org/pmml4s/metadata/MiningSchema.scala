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
package org.pmml4s.metadata

import org.pmml4s.common.{OpType, PmmlElement}
import org.pmml4s.model.Model
import org.pmml4s.util.Utils

import scala.collection.mutable

/**
 * Usage type
 *
 *  - active: field used as input (independent field).
 *  - target: field that was used a training target for supervised models.
 *  - predicted: field whose value is predicted by the model. As of PMML 4.2, this is deprecated and it has been
 * replaced by the usage type target.
 *  - supplementary: field holding additional descriptive information. Supplementary fields are not required to apply a
 * model. They are provided as additional information for explanatory purpose, though. When some field has gone through
 * preprocessing transformations before a model is built, then an additional supplementary field is typically used to
 * describe the statistics for the original field values.
 *  - group: field similar to the SQL GROUP BY. For example, this is used by AssociationModel and SequenceModel to group
 * items into transactions by customerID or by transactionID.
 *  - order: This field defines the order of items or transactions and is currently used in SequenceModel and
 * TimeSeriesModel. Similarly to group, it is motivated by the SQL syntax, namely by the ORDER BY statement.
 *  - frequencyWeight and analysisWeight: These fields are not needed for scoring, but provide very important information
 * on how the model was built. Frequency weight usually has positive integer values and is sometimes called "replication
 * weight". Its values can be interpreted as the number of times each record appears in the data. Analysis weight can
 * have fractional positive values, it could be used for regression weight in regression models or for case weight in
 * trees, etc. It can be interpreted as different importance of the cases in the model. Counts in ModelStats and
 * Partitions can be computed using frequency weight, mean and standard deviation values can be computed using both
 * weights.
 */
object UsageType extends Enumeration {
  type UsageType = Value
  val active, predicted, target, supplementary, group, order, frequencyWeight, analysisWeight = Value
}

/**
 * Outliers
 *
 *  - asIs: field values treated at face value.
 *  - asMissingValues: outlier values are treated as if they were missing.
 *  - asExtremeValues: outlier values are changed to a specific high or low value defined in MiningField.
 */
object OutlierTreatmentMethod extends Enumeration {
  type OutlierTreatmentMethod = Value
  val asIs, asMissingValues, asExtremeValues = Value
}

/**
 * In a PMML consumer this field is for information only, unless the value is returnInvalid, in which case if a missing
 * value is encountered in the given field, the model should return a value indicating an invalid result; otherwise,
 * the consumer only looks at missingValueReplacement - if a value is present it replaces missing values. Except as
 * described above, the missingValueTreatment attribute just indicates how the missingValueReplacement was derived, but
 * places no behavioral requirement on the consumer.
 */
object MissingValueTreatment extends Enumeration {
  type MissingValueTreatment = Value
  val asIs, asMean, asMode, asMedian, asValue, returnInvalid = Value
}

/**
 * This field specifies how invalid input values are handled.
 *
 *  - returnInvalid is the default and specifies that, when an invalid input is encountered, the model should return a
 * value indicating an invalid result has been returned.
 *  - asIs means to use the input without modification.
 *  - asMissing specifies that an invalid input value should be treated as a missing value and follow the behavior
 * specified by the missingValueReplacement attribute if present (see above).
 * If asMissing is specified but there is no respective missingValueReplacement present, a missing value is passed on
 * for eventual handling by successive transformations via DerivedFields or in the actual mining model.
 * - asValue specifies that an invalid input value should be replaced with the value specified by attribute
 * invalidValueReplacement which must be present in this case, or the PMML is invalid.
 */
object InvalidValueTreatment extends Enumeration {
  type InvalidValueTreatment = Value
  val returnInvalid, asIs, asMissing, asValue = Value
}

import org.pmml4s.metadata.InvalidValueTreatment._
import org.pmml4s.metadata.MissingValueTreatment._
import org.pmml4s.metadata.OutlierTreatmentMethod._
import org.pmml4s.metadata.UsageType._

trait HasUsageType {

  /** Usage type */
  def usageType: UsageType

  /** Tests whether this field is input, true for active and group that is used by the association model. */
  def isInput: Boolean = usageType == UsageType.active || usageType == UsageType.group

  /** Tests whether this field is target, true for target and predicted */
  def isTarget: Boolean = (usageType == UsageType.target || usageType == UsageType.predicted)
}

/**
 * MiningFields also define the usage of each field (active, supplementary, target, ...) as well as policies for
 * treating missing, invalid or outlier values.
 *
 * @param name                    Symbolic name of field, must refer to a field in the scope of the parent of the
 *                                MiningSchema's model element.
 * @param usageType
 * @param opType                  The attribute value overrides the corresponding value in the DataField. That is, a
 *                                DataField can be used with different optypes in different models. For example, a 0/1
 *                                indicator could be used as a numeric input field in a regression model while the same
 *                                field is used as a categorical field in a tree model.
 * @param importance              States the relative importance of the field.
 * @param outliers
 * @param lowValue
 * @param highValue
 * @param missingValueReplacement If this attribute is specified then a missing input value is automatically replaced by
 *                                the given value. That is, the model itself works as if the given value was found in
 *                                the original input. For example the surrogate operator in TreeModel does not apply if
 *                                the MiningField specifies a replacement value.
 * @param missingValueTreatment   This field is for information only.
 * @param invalidValueTreatment   Specifies how invalid input values are handled.
 * @param invalidValueReplacement
 */
case class MiningField(
                        val name: String,
                        val usageType: UsageType,
                        val opType: Option[OpType],
                        val importance: Option[Double] = None,
                        val outliers: OutlierTreatmentMethod = OutlierTreatmentMethod.asIs,
                        val lowValue: Option[Double] = None,
                        val highValue: Option[Double] = None,
                        val missingValueReplacement: Option[Any] = None,
                        val missingValueTreatment: Option[MissingValueTreatment] = None,
                        val invalidValueTreatment: InvalidValueTreatment = InvalidValueTreatment.returnInvalid,
                        val invalidValueReplacement: Option[Any] = None)
  extends HasUsageType with PmmlElement {

  /* Checks if the mining field has any value preprocess operations defined. */
  def isDefault: Boolean = (outliers == OutlierTreatmentMethod.asIs &&
    missingValueReplacement.isEmpty &&
    !missingValueTreatment.contains(MissingValueTreatment.returnInvalid) &&
    invalidValueTreatment == InvalidValueTreatment.returnInvalid &&
    invalidValueReplacement.isEmpty)
}
/**
 * The MiningSchema is the Gate Keeper for its model element. All data entering a model must pass through the MiningSchema.
 * Each model element contains one MiningSchema which lists fields as used in that model. While the MiningSchema contains information
 * that is specific to a certain model, the DataDictionary contains data definitions which do not vary per model.
 * The main purpose of the MiningSchema is to list the fields that have to be provided in order to apply the model.
 */
class MiningSchema(val miningFields: Array[MiningField]) extends HasTargetFields with PmmlElement {
  private[this] val nameToField = Utils.toMap(names, miningFields)

  val inputMiningFields: Array[MiningField] = for (name <- inputNames) yield apply(name)

  override def targetNames: Array[String] = miningFields.filter(_.isTarget).map(_.name)

  def inputNames: Array[String] = miningFields.filter(_.isInput).map(_.name)

  def names: Array[String] = miningFields.map(_.name)

  def get(name: String): Option[MiningField] = nameToField.get(name)

  def contains(name: String): Boolean = nameToField.contains(name)

  def apply(name: String): MiningField = nameToField(name)

  def apply(i: Int): MiningField = miningFields(i)

  def getInput(i: Int): MiningField = inputMiningFields(i)

  def getByUsageType(typ: UsageType): Array[MiningField] = miningFields.filter(_.usageType == typ)

  def importances: Map[String, Double] = if (inputMiningFields.length > 0 && inputMiningFields(0).importance.isDefined) {
    inputMiningFields.map(x => (x.name, x.importance.getOrElse(0.0))).toMap
  } else Map.empty
}

trait HasMiningSchema {
  self: Model =>
  def miningSchema: MiningSchema
}



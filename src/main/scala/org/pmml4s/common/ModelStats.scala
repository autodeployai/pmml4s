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

import org.pmml4s.metadata.Field
import org.pmml4s.model.Model

/**
 * Provides a basic framework for representing variable statistics.
 */
class ModelStats extends PmmlElement {
  ???
}

trait HasModelStats {
  self: Model =>
  def modelStats: Option[ModelStats]
}


/**
 * A Partition contains statistics for a subset of records, for example it can describe the population in a cluster.
 * The content of a Partition mirrors the definition of the general univariate statistics. That is, each Partition
 * describes the distribution per field. For each field there can be information about frequencies, numeric moments,
 * etc.
 *
 * The attribute name identifies the Partition. The attribute size is the number of records. All aggregates in
 * PartitionFieldStats must have size = totalFrequency in Counts if specified.
 */
class Partition(
                 val name: String,
                 val partitionFieldStats: Array[PartitionFieldStats],
                 val size: Option[Double] = None) extends PmmlElement

/**
 * field references to (the name of) a MiningField for background statistics. The sequence of NUM-ARRAYs is the same as
 * for ContStats. For categorical fields there is only one array containing the frequencies; for numeric fields, the
 * second and third array contain the sums of values and the sums of squared values, respectively. The number of values
 * in each array must match the number of categories or intervals in UnivariateStats of the field.
 */
class PartitionFieldStats(
                           val field: Field,
                           val frequencies: Array[Double],
                           val weighted: Boolean = false,
                           val counts: Option[Counts] = None,
                           val numericInfo: Option[NumericInfo] = None,
                           val sumValues: Option[Array[Double]] = None,
                           val sumSquaredValues: Option[Array[Double]] = None) extends PmmlElement

/**
 * The values for mean, minimum, maximum and standardDeviation are defined as usual. median is calculated as the 50%
 * quantile; interQuartileRange is calculated as (75% quantile - 25% quantile).
 */
class NumericInfo(
                   val quantiles: Array[Quantile],
                   val minimum: Option[Double] = None,
                   val maximum: Option[Double] = None,
                   val mean: Option[Double] = None,
                   val standardDeviation: Option[Double] = None,
                   val median: Option[Double] = None,
                   val interQuartileRange: Option[Double] = None) extends PmmlElement

/**
 * @param quantileLimit A percentage number between 0 and 100
 * @param quantileValue The corresponding value in the domain of field values.
 */
class Quantile(val quantileLimit: Double, val quantileValue: Double) extends PmmlElement

/**
 * Carries counters for frequency of values with respect to their state of being missing, invalid, or valid. The counts
 * can be non-integer if they are weighted.
 *
 * @param totalFreq   Counts all records, same as for statistics of all MiningFields.
 * @param missingFreq Counts the number of records where value is missing.
 * @param invalidFreq Counts the number of records with values other than valid. The total frequency includes the
 *                    missing values and invalid values.
 * @param cardinality The number of unique, or distinct, values that the variable has.
 */
class Counts(
              val totalFreq: Double,
              val missingFreq: Option[Double] = None,
              val invalidFreq: Option[Double] = None,
              val cardinality: Option[Double] = None) extends PmmlElement
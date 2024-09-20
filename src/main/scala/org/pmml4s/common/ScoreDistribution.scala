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

import org.pmml4s.data.DataVal

/**
 * Comprises a method to list predicted values in a classification trees structure.
 */
class ScoreDistribution(
                         val value: DataVal,
                         val recordCount: Double,
                         val confidence: Option[Double],
                         val probability: Option[Double]) extends PmmlElement {

  /** Copy with a new probability. */
  def withProbability(probability: Double) = new ScoreDistribution(value, recordCount, confidence, Some(probability))

  /** Copy with a new confidence. */
  def withConfidence(confidence: Double) = new ScoreDistribution(value, recordCount, Some(confidence), probability)
}

trait HasScoreDistributions {
  def scoreDistributions: ScoreDistributions

  def getConfidence(value: DataVal): Double = scoreDistributions.confidences.getOrElse(value, Double.NaN)

  def getProbability(value: DataVal): Double = scoreDistributions.probabilities.getOrElse(value, Double.NaN)

  def probabilities: Map[DataVal, Double] = scoreDistributions.probabilities
}

class ScoreDistributions(val scoreDistributions: Array[ScoreDistribution]) extends PmmlElement {
  val valueToDistribution: Map[DataVal, ScoreDistribution] = {
    val total = scoreDistributions.map(_.recordCount).sum

    // Compute probabilities if not present.
    (if (total > 0.0) {
      scoreDistributions.map { x =>
        if (x.probability.isDefined) x else {
          (x.withProbability(x.recordCount / total))
        }
      }
    } else scoreDistributions).map(x => (x.value, x)).toMap
  }

  val probabilities: Map[DataVal, Double] = valueToDistribution.map(x => (x._1, x._2.probability.getOrElse(Double.NaN)))

  val confidences: Map[DataVal, Double] = valueToDistribution.map(x => (x._1, x._2.confidence.getOrElse(Double.NaN)))

  def this() = {
    this(Array.empty)
  }

  def classes: Array[DataVal] = scoreDistributions.map(_.value)
}
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

import org.apache.commons.math3.special.Gamma
import org.pmml4s.common.ContinuousDistributionType.ContinuousDistributionType
import org.pmml4s.xml.ElemTags

object ContinuousDistributionType extends Enumeration {
  type ContinuousDistributionType = Value
  val ANY, GAUSSIAN, POISSON, UNIFORM = Value
}

trait ContinuousDistribution extends PmmlElement {
  def probability(x: Double): Double

  def distType: ContinuousDistributionType
}

object ContinuousDistribution {

  import ElemTags._

  val values = Set(ANY_DISTRIBUTION, GAUSSIAN_DISTRIBUTION, POISSON_DISTRIBUTION, UNIFORM_DISTRIBUTION)

  def contains(s: String) = values.contains(s)
}

class AnyDistribution(val mean: Double, val variance: Double) extends ContinuousDistribution {
  override def probability(x: Double): Double = ???

  override def distType: ContinuousDistributionType = ContinuousDistributionType.ANY
}

class GaussianDistribution(val mean: Double, val variance: Double) extends ContinuousDistribution {
  private val beta = Math.sqrt(2.0 * Math.PI * variance)

  override def probability(x: Double): Double = {
    val a = x - mean
    Math.exp(-0.5 * (a * a) / variance) / beta
  }

  override def distType: ContinuousDistributionType = ContinuousDistributionType.GAUSSIAN
}

class PoissonDistribution(val mean: Double) extends ContinuousDistribution {
  override def probability(x: Double): Double = {
    Math.pow(mean, x) * Math.exp(-mean) / Gamma.digamma(x + 1)
  }

  override def distType: ContinuousDistributionType = ContinuousDistributionType.POISSON
}

class UniformDistribution(val lower: Double, val upper: Double) extends ContinuousDistribution {
  override def probability(x: Double): Double = ???

  override def distType: ContinuousDistributionType = ContinuousDistributionType.UNIFORM
}

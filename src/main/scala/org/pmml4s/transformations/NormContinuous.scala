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

import org.pmml4s.common.PmmlElement
import org.pmml4s.data.Series
import org.pmml4s.metadata.OutlierTreatmentMethod.OutlierTreatmentMethod
import org.pmml4s.metadata.{Field, OutlierTreatmentMethod}
import org.pmml4s.util.Utils

/**
 * Normalization provides a basic framework for mapping input values to specific value ranges, usually the numeric range
 * [0 .. 1]. Normalization is used, e.g., in neural networks and clustering models.
 *
 * Defines how to normalize an input field by piecewise linear interpolation. The mapMissingTo attribute defines the
 * value the output is to take if the input is missing. If the mapMissingTo attribute is not specified, then missing
 * input values produce a missing result.
 */
class NormContinuous(
                      val linearNorms: Array[LinearNorm],
                      val field: Field,
                      val mapMissingTo: Option[Double],
                      val outliers: OutlierTreatmentMethod = OutlierTreatmentMethod.asIs)
  extends NumericFieldExpression {

  private lazy val slopes = for (i <- 1 until linearNorms.size) yield {
    (linearNorms(i).norm - linearNorms(i - 1).norm) / (linearNorms(i).orig - linearNorms(i - 1).orig)
  }

  private lazy val intercepts = for (i <- 1 until linearNorms.size) yield {
    (linearNorms(i - 1).norm * linearNorms(i).orig - linearNorms(i).norm * linearNorms(i - 1).orig) /
      (linearNorms(i).orig - linearNorms(i - 1).orig)
  }

  override def eval(series: Series): Double = {
    val res = super.eval(series)
    if (Utils.isMissing(res)) {
      mapMissingTo.getOrElse(Double.NaN)
    } else {
      if (res < linearNorms.head.orig || res > linearNorms.last.orig) {
        import OutlierTreatmentMethod._
        outliers match {
          case `asIs`            => {
            val pair = if (res < linearNorms.head.orig) (slopes.head, intercepts.head) else (slopes.last, intercepts.last)
            normalize(res, pair)
          }
          case `asMissingValues` => Double.NaN
          case `asExtremeValues` => {
            if (res < linearNorms.head.orig) linearNorms.head.norm else linearNorms.last.norm
          }
        }
      } else {
        normalize(res, findOrig(res))
      }
    }
  }

  override def deeval(input: Any): Double = {
    val d = input.asInstanceOf[Double]
    if (Utils.isMissing(d)) {
      Double.NaN
    } else {
      if (d < linearNorms.head.norm || d > linearNorms.last.norm) {
        import OutlierTreatmentMethod._
        outliers match {
          case `asIs`            => {
            val pair = if (d < linearNorms.head.norm) (slopes.head, intercepts.head) else (slopes.last, intercepts.last)
            denormalize(d, pair)
          }
          case `asMissingValues` => Double.NaN
          case `asExtremeValues` => {
            if (d < linearNorms.head.norm) linearNorms.head.orig else linearNorms.last.orig
          }
        }
      } else {
        denormalize(d, findNorm(d))
      }
    }
  }

  private def findOrig(value: Double): (Double, Double) = {
    var i = 0
    while (i < linearNorms.size - 1) {
      if (value >= linearNorms(i).orig && value <= linearNorms(i + 1).orig) {
        return (slopes(i), intercepts(i))
      }
      i += 1
    }

    (Double.NaN, Double.NaN)
  }

  private def findNorm(value: Double): (Double, Double) = {
    var i = 0
    while (i < linearNorms.size - 1) {
      if (value >= linearNorms(i).norm && value <= linearNorms(i + 1).norm) {
        return (slopes(i), intercepts(i))
      }
      i += 1
    }

    (Double.NaN, Double.NaN)
  }

  private def normalize(x: Double, pair: (Double, Double)): Double = {
    x * pair._1 + pair._2
  }

  private def denormalize(x: Double, pair: (Double, Double)): Double = {
    (x - pair._2) / pair._1
  }
}

class LinearNorm(val orig: Double, val norm: Double) extends PmmlElement

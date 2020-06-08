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

import org.pmml4s.common.CompareFunction.CompareFunction
import org.pmml4s.common.ComparisonMeasureKind.ComparisonMeasureKind
import org.pmml4s.xml.ElemTags

class ComparisonMeasure(
                         val kind: ComparisonMeasureKind,
                         val distance: Distance,
                         val compareFunction: CompareFunction = CompareFunction.absDiff,
                         val minimum: Option[Double] = None,
                         val maximum: Option[Double] = None) extends PmmlElement

object ComparisonMeasureKind extends Enumeration {
  type ComparisonMeasureKind = Value
  val distance, similarity = Value
}

trait Distance extends PmmlElement {
  def distance(nonMissing: Array[Int],
               fs: Array[CompareFunction],
               xs: Array[Double],
               ys: Array[Double],
               weights: Array[Double],
               adjustM: Double = 1.0,
               s: Array[Double]): Double

  def matrix(xs: Array[Double], ys: Array[Double]) = {
    var a11, a10, a01, a00 = 0.0
    var i = 0
    while (i < xs.length) {
      if (xs(i) == 0 && ys(i) == 0) {
        a00 += 1
      } else if (xs(i) == 0 && ys(i) == 1) {
        a01 += 1
      } else if (xs(i) == 1 && ys(i) == 0) {
        a10 += 1
      } else if (xs(i) == 1 && ys(i) == 1) {
        a11 += 1
      }
      i += 1
    }

    (a11, a10, a01, a00)
  }

  def compare(fun: CompareFunction, x: Double, y: Double, s: Double = Double.NaN): Double = {
    import CompareFunction._
    fun match {
      case `absDiff`  => Math.abs(x - y)
      case `gaussSim` => Math.exp(-Math.log(2) * (x - y) * (x - y) / (s * s))
      case `delta`    => if (x == y) 0.0 else 1.0
      case `equal`    => if (x == y) 1.0 else 0.0
      case `table`    => ??? // TODO: table
    }
  }
}

object Distance {

  import ElemTags._

  val values = Set(EUCLIDEAN, SQUARED_EUCLIDEAN, CHEBYCHEV, CITY_BLOCK, MINKOWSKI, SIMPLE_MATCHING, JACCARD, TANIMOTO,
    BINARY_SIMILARITY)

  def contains(s: String) = values.contains(s)

}

object euclidean extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    var sum = 0.0
    for (i <- nonMissing) {
      val d = compare(fs(i), xs(i), ys(i), s(i))
      sum += (d * d * weights(i))
    }

    Math.sqrt(sum * adjustM)
  }
}

object squaredEuclidean extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    var sum = 0.0
    for (i <- nonMissing) {
      val d = compare(fs(i), xs(i), ys(i), s(i))
      sum += (d * d * weights(i))
    }

    sum * adjustM
  }
}

object chebychev extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    val arr = for (i <- nonMissing) yield compare(fs(i), xs(i), ys(i), s(i)) * weights(i)
    arr.max * adjustM
  }
}

object cityBlock extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    var sum = 0.0
    for (i <- nonMissing) {
      sum += (compare(fs(i), xs(i), ys(i), s(i)) * weights(i))
    }

    sum * adjustM
  }
}

class minkowski(val p: Double) extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    var sum = 0.0
    for (i <- nonMissing) {
      val d = Math.pow(compare(fs(i), xs(i), ys(i), s(i)), p)
      sum += (d * weights(i))
    }

    Math.pow(sum * adjustM, 1 / p)
  }
}

object simpleMatching extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    val (a11, a10, a01, a00) = matrix(xs, ys)
    (a11 + a00) / (a11 + a10 + a01 + a00)
  }
}

object jaccard extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    val (a11, a10, a01, a00) = matrix(xs, ys)
    (a11) / (a11 + a10 + a01)
  }
}

object tanimoto extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    val (a11, a10, a01, a00) = matrix(xs, ys)
    (a11 + a00) / (a11 + 2 * (a10 + a01) + a00)
  }
}

class binarySimilarity(val c00: Double,
                       val c01: Double,
                       val c10: Double,
                       val c11: Double,
                       val d00: Double,
                       val d01: Double,
                       val d10: Double,
                       val d11: Double) extends Distance {
  override def distance(nonMissing: Array[Int], fs: Array[CompareFunction], xs: Array[Double], ys: Array[Double],
                        weights: Array[Double], adjustM: Double, s: Array[Double]): Double = {
    val (a11, a10, a01, a00) = matrix(xs, ys)
    (c11 * a11 + c10 * a10 + c01 * a01 + c00 * a00) / (d11 * a11 + d10 * a10 + d01 * a01 + d00 * a00)
  }
}

/**
 *  - absDiff: absolute difference c(x,y) = |x-y|
 *
 *  - gaussSim: gaussian similarity c(x,y) = exp(-ln(2)*z*z/(s*s)) where z=x-y, and s is the value of attribute
 * similarityScale (required in this case) in the ClusteringField
 *
 *  - delta: c(x,y) = 0 if x=y, 1 else
 *
 *  - equal: c(x,y) = 1 if x=y, 0 else
 *
 *  - table: c(x,y) = lookup in similarity matrix
 */
object CompareFunction extends Enumeration {
  type CompareFunction = Value
  val absDiff, gaussSim, delta, equal, table = Value
}

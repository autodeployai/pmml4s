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
package org.pmml4s.model

import org.pmml4s.common.MiningFunction._
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata.{MiningSchema, Output, OutputField, Targets}
import org.pmml4s.model.RegressionModelType.RegressionModelType
import org.pmml4s.model.RegressionNormalizationMethod.RegressionNormalizationMethod
import org.pmml4s.transformations.{LocalTransformations, StdNormalCDF}
import org.pmml4s.util.Utils

import scala.collection.{immutable, mutable}

/**
 * The regression functions are used to determine the relationship between the dependent variable (target field) and one or more independent variables.
 * The dependent variable is the one whose values you want to predict, whereas the independent variables are the variables that you base your prediction
 * on. While the term regression usually refers to the prediction of numeric values, the PMML element RegressionModel can also be used for classification.
 * This is due to the fact that multiple regression equations can be combined in order to predict categorical values.
 */
class RegressionModel(
                       override var parent: Model,
                       override val attributes: RegressionAttributes,
                       override val miningSchema: MiningSchema,
                       val regressionTables: Array[RegressionTable],
                       override val output: Option[Output] = None,
                       override val targets: Option[Targets] = None,
                       override val localTransformations: Option[LocalTransformations] = None,
                       override val modelStats: Option[ModelStats] = None,
                       override val modelExplanation: Option[ModelExplanation] = None,
                       override val modelVerification: Option[ModelVerification] = None,
                       override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedRegressionAttributes {

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.RegressionModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = createOutputs()

    import RegressionNormalizationMethod._
    if (isRegression) {
      val y: Double = regressionTables.head.eval(series)
      val prediction: Double = if (Utils.isMissing(y)) Double.NaN else
        normalizationMethod match {
          case `none`    => y
          case `softmax` => 1.0 / (1.0 + Math.exp(-y))
          case `logit`   => 1.0 / (1.0 + Math.exp(-y))
          case `exp`     => Math.exp(y)
          case `probit`  => StdNormalCDF.eval(y)
          case `cloglog` => 1.0 - Math.exp(-Math.exp(y))
          case `cauchit` => 0.5 + (1 / Math.PI) * Math.atan(y)
          case _         => Double.NaN
        }
      outputs.predictedValue = if (Utils.isMissing(prediction)) null else prediction
    } else {
      val y = regressionTables.map(x => (x.targetCategory.get, x.eval(series))).toSeq
      val probabilities: Map[Any, Double] = if (!y.exists(x => Utils.isMissing(x._2))) {
        val pairs: Seq[(Any, Double)] = normalizationMethod match {
          case `softmax`   => {
            val yTransformed = y.map(x => (x._1, Math.exp(x._2)))
            val sum = yTransformed.map(_._2).sum
            for (elem <- yTransformed) yield (elem._1, elem._2 / sum)
          }
          case `simplemax` => {
            val sum = y.map(_._2).sum
            for (elem <- y) yield (elem._1, elem._2 / sum)
          }
          case `none`      => {
            if (isOrdinal) {
              val init = for (i <- 0 until y.length - 1) yield (y(i)._1, if (i == 0) y(i)._2 else (y(i)._2 - y(i - 1)._2))
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary) {
              val y1 = y.head._2
              val p1 = if (y1 < 0) 0.0 else if (y1 > 1) 1.0 else y1
              Seq((y.head._1, p1), (y.last._1, 1.0 - p1))
            } else {
              val init = y.init
              init :+ ((y.last._1, 1.0 - init.map(_._2).sum))
            }
          }
          case `logit`     => {
            if (isOrdinal) {
              val yTransformed = y.map(x => (x._1, 1.0 / (1.0 + Math.exp(-x._2))))
              val init = for (i <- 0 until yTransformed.length - 1) yield (yTransformed(i)._1, if (i == 0) yTransformed(i)._2 else yTransformed(i)._2 - yTransformed(i - 1)._2)
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary && y.length == 2) {
              // The trivial may not be the last element.
              val (first, second) = if (y.last._2 == 0.0) (y.head, y.last) else (y.last, y.head)
              val p1 = 1.0 / (1.0 + Math.exp(-first._2))
              Seq((first._1, p1), (second._1, 1.0 - p1))
            } else {
              Seq.empty
            }
          }
          case `probit`    => {
            if (isOrdinal) {
              val yTransformed = y.map(x => (x._1, StdNormalCDF.eval(x._2)))
              val init = for (i <- 0 until yTransformed.length - 1) yield (yTransformed(i)._1, if (i == 0) yTransformed(i)._2 else yTransformed(i)._2 - yTransformed(i - 1)._2)
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary) {
              // The trivial may not be the last element.
              val (first, second) = if (y.last._2 == 0.0) (y.head, y.last) else (y.last, y.head)
              val p1 = StdNormalCDF.eval(first._2)
              Seq((first._1, p1), (second._1, 1.0 - p1))
            } else {
              Seq.empty
            }
          }
          case `cloglog`   => {
            if (isOrdinal) {
              val yTransformed = y.map(x => (x._1, 1.0 - Math.exp(-Math.exp(x._2))))
              val init = for (i <- 0 until yTransformed.length - 1) yield (yTransformed(i)._1, if (i == 0) yTransformed(i)._2 else yTransformed(i)._2 - yTransformed(i - 1)._2)
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary) {
              // The trivial may not be the last element.
              val (first, second) = if (y.last._2 == 0.0) (y.head, y.last) else (y.last, y.head)
              val p1 = 1.0 - Math.exp(-Math.exp(first._2))
              Seq((first._1, p1), (second._1, 1.0 - p1))
            } else {
              Seq.empty
            }
          }
          case `cauchit`   => {
            val coeff = 1.0 / Math.PI
            if (isOrdinal) {
              val yTransformed = y.map(x => (x._1, 0.5 + coeff * Math.atan(x._2)))
              val init = for (i <- 0 until yTransformed.length - 1) yield (yTransformed(i)._1, if (i == 0) yTransformed(i)._2 else yTransformed(i)._2 - yTransformed(i - 1)._2)
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary) {
              // The trivial may not be the last element.
              val (first, second) = if (y.last._2 == 0.0) (y.head, y.last) else (y.last, y.head)
              val p1 = 0.5 + coeff * Math.atan(first._2)
              Seq((first._1, p1), (second._1, 1.0 - p1))
            } else {
              Seq.empty
            }
          }
          case `loglog`    => {
            if (isOrdinal) {
              val yTransformed = y.map(x => (x._1, Math.exp(-Math.exp(-x._2))))
              val init = for (i <- 0 until yTransformed.length - 1) yield (yTransformed(i)._1, if (i == 0) yTransformed(i)._2 else yTransformed(i)._2 - yTransformed(i - 1)._2)
              init :+ ((y.last._1, 1.0 - init.last._2))
            } else if (isBinary) {
              // The trivial may not be the last element.
              val (first, second) = if (y.last._2 == 0.0) (y.head, y.last) else (y.last, y.head)
              val p1 = Math.exp(-Math.exp(-first._2))
              Seq((first._1, p1), (second._1, 1.0 - p1))
            } else {
              Seq.empty
            }
          }
          case _           => Seq.empty
        }
        pairs.toMap
      } else {
        targets.map(_.priorProbabilities).getOrElse(Map.empty)
      }

      outputs.probabilities = probabilities
      outputs.evalPredictedValueByProbabilities()
    }

    result(series, outputs)
  }

  override def inferClasses: Array[Any] = if (isClassification) {
    regressionTables.map(x => x.targetCategory.get)
  } else {
    super.inferClasses
  }

  /** Returns all candidates output fields of this model when there is no output specified explicitly. */
  override def defaultOutputFields: Array[OutputField] = {
    val res = mutable.ArrayBuilder.make[OutputField]
    res.sizeHint(if (isClassification) numClasses + 1 else 1)
    res += OutputField.predictedValue(this)
    if (isClassification) {
      res += OutputField.probability()
      for (cls <- classes) {
        res += OutputField.probability(cls)
      }
    }

    res.result()
  }

  /** Creates an object of RegressionOutputs that is for writing into an output series.  */
  override def createOutputs(): RegressionOutputs = new RegressionOutputs
}

/**
 * Specifies the type of a regression model. The attribute modelType is for information only.
 */
object RegressionModelType extends Enumeration {
  type RegressionModelType = Value
  val linearRegression, stepwisePolynomialRegression, logisticRegression = Value
}

/**
 * Describes how the prediction is converted into a confidence value (aka probability).
 */
object RegressionNormalizationMethod extends Enumeration {
  type RegressionNormalizationMethod = Value
  val none, simplemax, softmax, logit, probit, cloglog, exp, loglog, cauchit = Value
}

trait HasRegressionAttributes extends HasModelAttributes {

  def modelType: Option[RegressionModelType]

  /** The name of the target field (also called dependent variable). The attribute targetFieldName is for information only. */
  def targetFieldName: Option[String]

  def normalizationMethod: RegressionNormalizationMethod
}

trait HasWrappedRegressionAttributes extends HasWrappedModelAttributes with HasRegressionAttributes {

  override def attributes: RegressionAttributes

  def modelType: Option[RegressionModelType] = attributes.modelType

  def targetFieldName: Option[String] = attributes.targetFieldName

  def normalizationMethod: RegressionNormalizationMethod = attributes.normalizationMethod
}

class RegressionAttributes(
                            override val functionName: MiningFunction,
                            override val modelName: Option[String] = None,
                            override val algorithmName: Option[String] = None,
                            override val isScorable: Boolean = true,
                            val modelType: Option[RegressionModelType] = None,
                            val targetFieldName: Option[String] = None,
                            val normalizationMethod: RegressionNormalizationMethod = RegressionNormalizationMethod.none
                          ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable) with HasRegressionAttributes

class RegressionOutputs extends MixedClsWithRegOutputs

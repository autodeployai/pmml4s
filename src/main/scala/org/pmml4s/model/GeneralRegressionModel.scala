/*
 * Copyright (c) 2017-2024 AutoDeployAI
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

import org.pmml4s.PmmlDeprecated
import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common._
import org.pmml4s.data.{DataVal, Series}
import org.pmml4s.metadata.{Field, MiningSchema, Output, Targets}
import org.pmml4s.model.CumulativeLinkFunction.CumulativeLinkFunction
import org.pmml4s.model.Distribution.Distribution
import org.pmml4s.model.GeneralModelType.GeneralModelType
import org.pmml4s.model.LinkFunction.LinkFunction
import org.pmml4s.model.PCovMatrixType.PCovMatrixType
import org.pmml4s.transformations.{LocalTransformations, StdNormalCDF}
import org.pmml4s.util.{MathUtils, Utils}

import scala.collection.{immutable, mutable}

/**
 * Definition of a general regression model. As the name says it, this is intended to support a multitude of
 * regression models.
 */
class GeneralRegressionModel(
                              var parent: Model,
                              override val attributes: GeneralRegressionAttributes,
                              override val miningSchema: MiningSchema,
                              val parameterList: ParameterList,
                              val factorList: Option[FactorList],
                              val covariateList: Option[CovariateList],
                              val ppMatrix: PPMatrix,
                              val pCovMatrix: Option[PCovMatrix],
                              val paramMatrix: ParamMatrix,
                              val eventValues: Option[EventValues],
                              val baseCumHazardTables: Option[BaseCumHazardTables],
                              override val output: Option[Output] = None,
                              override val targets: Option[Targets] = None,
                              override val localTransformations: Option[LocalTransformations] = None,
                              override val modelStats: Option[ModelStats] = None,
                              override val modelExplanation: Option[ModelExplanation] = None,
                              override val modelVerification: Option[ModelVerification] = None,
                              override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedGeneralRegressionAttributes {

  override val targetReferenceCategory: Option[DataVal] = attributes.targetReferenceCategory.map(targetField.toVal)

  private[this] val parameters: Array[RegressionParameter] = parameterList.parameters.map(x => {
    val cells = ppMatrix.getPPCells(x.name)
    val factors = mutable.ArrayBuilder.make[FactorPredictor]
    val covariances = mutable.ArrayBuilder.make[CovariatePredictor]
    factors.sizeHint(cells.length)
    covariances.sizeHint(cells.length)
    cells.map(y => {
      val name = y.predictorName.name
      y.toPredictor(factorList.flatMap(_.get(name)).orNull) match {
        case f: FactorPredictor    => factors += f
        case c: CovariatePredictor => covariances += c
        case _                     => ???
      }
    })
    new RegressionParameter(factors.result(), covariances.result())
  })

  private[this] val betas: Map[DataVal, (Array[Int], Array[Double])] = {
    val res = paramMatrix.cells.groupBy(_.targetCategory).
      map(x => ((if (x._1.isDefined) x._1.get else null), (x._2.map(c => parameterList.indexOf(c.parameterName)), x._2.map(_.beta))))

    if (isClassification && res.size == numClasses - 1) {
      val miss: DataVal = targetReferenceCategory.getOrElse({
        val missSet = classes.toSet -- res.keys
        if (missSet.size == 1) {
          missSet.head
        } else null
      })

      if (miss != null) {
        res + (miss -> (Array.emptyIntArray, Array.emptyDoubleArray))
      } else res
    } else res
  }

  private[this] val s: Double = if (modelType == GeneralModelType.CoxRegression) {
    MathUtils.product(parameterList.parameters.map(_.referencePoint), paramMatrix.cells.map(_.beta))
  } else Double.NaN

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.GeneralRegressionModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val outputs = new GeneralRegressionOutputs
    val xs = parameters.map(_.eval(series))
    import GeneralModelType._
    modelType match {
      case `regression` | `generalLinear` => {
        val rs = betas.map(x => (x._1, MathUtils.product(x._2._1.map(ind => xs(ind)), x._2._2)))
        outputs.setPredictedValue(rs.head._2)
      }
      case `multinomialLogistic`          => {
        val rs = betas.map(x => (x._1, MathUtils.product(x._2._1.map(ind => xs(ind)), x._2._2)))
        outputs.setPredictedValue(rs.maxBy(_._2)._1)

        if (!isPredictionOnly) {
          val ss = rs.map(x => (x._1, rs.map(s => s._2 - x._2)))
          val probabilities = ss.map(x => (x._1, {
            // overflow
            if (x._2.exists(_ > 700)) {
              0.0
            } else {
              1.0 / (x._2.map(Math.exp).sum)
            }
          }))

          outputs.setProbabilities(probabilities)
          outputs.evalPredictedValueByProbabilities(classes)
        }
      }
      case `ordinalMultinomial`           => {
        val a = offsetVariable.map(_.getDouble(series)).getOrElse(offsetValue.getOrElse(0.0))
        val rs = betas.map(x => (x._1, MathUtils.product(x._2._1.map(ind => xs(ind)), x._2._2)))
        val extra = rs.find(x => x._1 == null)
        val ys = (if (extra.isDefined) {
          rs.filter(x => x._1 != null).map(x => targetField.encode(x._1) -> (x._2 + extra.get._2))
        } else {
          rs.map(x => targetField.encode(x._1) -> x._2)
        }).toArray.sortBy(_._1).map(_._2)

        import CumulativeLinkFunction._
        val fs = cumulativeLink.get match {
          case `logit`   => ys.map(y => 1.0 / (1.0 + Math.exp(-y)))
          case `probit`  => ys.map(y => StdNormalCDF.eval(y))
          case `cloglog` => ys.map(y => 1 - Math.exp(-Math.exp(y)))
          case `loglog`  => ys.map(y => Math.exp(-Math.exp(y)))
          case `cauchit` => ys.map(y => 0.5 + (1 / Math.PI) * Math.atan(y))
        }
        if (isBinary) {
          outputs.setProbabilities(Map(classes(0) -> fs(0)))
          outputs.evalPredictedValueByProbabilities(classes)
        } else {
          val probabilities = (for (i <- 0 until fs.length) yield {
            classes(i) -> (if (i == 0) fs(i) else fs(i) - fs(i - 1))
          }).toMap
          outputs.setProbabilities(probabilities)
          outputs.evalPredictedValueByProbabilities(classes)
        }
      }
      case `generalizedLinear`            => {
        val a = offsetVariable.map(_.getDouble(series)).getOrElse(offsetValue.getOrElse(0.0))
        val b: Int = trialsVariable.map(_.getDouble(series).toInt).getOrElse(trialsValue.getOrElse(1))
        val y = betas.map(x => MathUtils.product(x._2._1.map(ind => xs(ind)), x._2._2)).head + a

        import LinkFunction._
        val predictedValue = b * (linkFunction.get match {
          case `cloglog`   => 1 - Math.exp(-Math.exp(y))
          case `identity`  => y
          case `log`       => Math.exp(y)
          case `logc`      => 1 - Math.exp(y)
          case `logit`     => 1 / (1 + Math.exp(-y))
          case `loglog`    => Math.exp(-Math.exp(-y))
          case `negbin`    => {
            val c = distParameter.get
            1 / (c * (Math.exp(-y) - 1))
          }
          case `oddspower` => {
            val d = linkParameter.get
            if (d != 0) {
              1 / (1 + Math.pow(1 + d * y, -1 / d))
            } else {
              1 / (1 + Math.exp(-y))
            }
          }
          case `power`     => {
            val d = linkParameter.get
            if (d != 0) {
              Math.pow(y, 1 / d)
            } else {
              Math.exp(y)
            }
          }
          case `probit`    => {
            StdNormalCDF.eval(y)
          }
        })

        if (distribution.isDefined && distribution.get == Distribution.binomial && isClassification) {
          val p1 = if (predictedValue < 0) 0 else if (predictedValue > 1) 1 else predictedValue
          outputs.setProbabilities(Map(betas.head._1 -> p1))
            .evalPredictedValueByProbabilities(classes)
        } else {
          outputs.setPredictedValue(predictedValue)
        }
      }
      case `CoxRegression`                => {
        var survival = Double.NaN
        var hazard = Double.NaN
        val tables = baseCumHazardTables.get
        val (maxTime, minTime: Double, baselineCells) = if (baselineStrataVariable.isDefined) {
          val value = baselineStrataVariable.map(_.get(series)).get
          val baselineStratum = tables.getBaselineStratum(value)
          if (baselineStratum.isEmpty) {
            return nullSeries
          }
          (baselineStratum.get.maxTime, baselineStratum.get.cells.head.time, baselineStratum.get.cells)
        } else {
          (tables.maxTime.get, tables.baselineCells.head.time, tables.baselineCells)
        }

        val endTime = endTimeVariable.map(_.getDouble(series)).get
        if (endTime < minTime) {
          survival = 1
          hazard = 0
        } else if (endTime > maxTime) {
          return nullSeries
        } else {
          val cell = findBaselineCell(baselineCells, endTime)
          var h0 = cell.cumHazard
          val r = betas.map(x => (x._1, MathUtils.product(x._2._1.map(ind => xs(ind)), x._2._2)))
          hazard = h0 * Math.exp(r.head._2 - s)
          survival = Math.exp(-hazard)
        }
        outputs.setPredictedValue(survival)
      }
    }

    result(series, outputs)
  }

  def findBaselineCell(baselineCells: Array[BaselineCell], endTime: Double): BaselineCell = {
    for (i <- baselineCells.length - 1 to 0) {
      if (baselineCells(i).time <= endTime) {
        return baselineCells(i)
      }
    }
    baselineCells(0)
  }

  /** Creates an object of GeneralRegressionOutputs that is for writing into an output series.  */
  override def createOutputs(): ModelOutputs = new GeneralRegressionOutputs
}

/**
 * Each Parameter contains a required name and optional label.
 *
 * @param name           Should be unique within the model and as brief as possible (since Parameter names appear
 *                       frequently in the document).
 * @param label          If present, is meant to give a hint on a Parameter's correlation with the Predictors.
 * @param referencePoint The optional attribute referencePoint is used in Cox regression models only and has a default
 *                       value of 0
 */
class Parameter(val name: String, val label: Option[String] = None, val referencePoint: Double = 0.0) extends PmmlElement

/**
 * Lists all Parameters. ParameterList can be empty only for CoxRegression models, for other models at least one
 * Parameter should be present.
 */
class ParameterList(val parameters: Array[Parameter]) extends PmmlElement {
  private lazy val nameToIndex: Map[String, Int] = parameters.zipWithIndex.map(x => (x._1.name, x._2)).toMap

  def size: Int = parameters.length

  def indexOf(name: String): Int = nameToIndex(name)
}

/**
 * Describes a categorical (factor) or a continuous (covariate) predictor for the model. When describing a factor, it
 * can optionally contain a list of categories and a contrast matrix. Such matrix describes the codings of categorical
 * variables. If a categorical variable has n values, there will be n rows and n-1 or n columns in the matrix. The rows
 * and columns correspond to the categories of the factor in the order listed in the Category element if it is present,
 * otherwise in the order listed in the DataField or DerivedField element. If the Categories element is present and the
 * corresponding DataField or DerivedField element has a list of valid categories, then the list in Categories should
 * be a subset of that in DataField or DerivedField. A contrast matrix with n-1 columns helps to reduce the total
 * number of parameters in the model.
 */
class Predictor(
                 val name: String,
                 val contrastMatrixType: Option[String] = None,
                 val categories: Option[Categories] = None,
                 val matrix: Option[Matrix] = None) extends PmmlElement

/**
 * List of factor (categorical predictor) names. Not present if this particular regression flavor does not support
 * factors (ex. linear regression). If present, the list may or may not be empty. Each name in the list must match a
 * DataField name or a DerivedField name. The factors must be categorical variables.
 */
class FactorList(val predictors: Array[Predictor]) extends PmmlElement {
  private lazy val nameToPredictor: Map[String, Predictor] = predictors.map(x => (x.name, x)).toMap

  def get(name: String): Option[Predictor] = nameToPredictor.get(name)
}

class Category(val value: DataVal) extends PmmlElement

class Categories(val categories: Array[Category]) extends PmmlElement {
  require(categories.length > 0, "At least one category is required.")

  lazy val valueToIndex: Map[DataVal, Int] = categories.map(_.value).zipWithIndex.toMap
}

/**
 * List of covariate names. Will not be present when there is no covariate. Each name in the list must match a
 * DataField name or a DerivedField name. The covariates will be treated as continuous variables.
 */
class CovariateList(val predictors: Array[Predictor]) extends PmmlElement {
  private lazy val nameToPredictor: Map[String, Predictor] = predictors.map(x => (x.name, x)).toMap

  def get(name: String): Option[Predictor] = nameToPredictor.get(name)
}

/**
 * Predictor-to-Parameter correlation matrix. It is a rectangular matrix having a column for each Predictor (factor or
 * covariate) and a row for each Parameter. The matrix is represented as a sequence of cells, each cell containing a
 * number representing the correlation between the Predictor and the Parameter.
 */
class PPMatrix(val cells: Array[PPCell]) extends PmmlElement {
  def getPPCells(name: String): Array[PPCell] = {
    cells.filter(x => x.parameterName == name)
  }
}

/** Cell in the PPMatrix. Knows its row name, column name. */
class PPCell(val value: DataVal,
             val predictorName: Field,
             val parameterName: String,
             val targetCategory: Option[DataVal] = None) extends PmmlElement {

  def toPredictor(p: Predictor): RegressionPredictor = if (predictorName.isCategorical) {
    if (p != null && p.matrix.isDefined && p.categories.isDefined) {
      new ContrastMatrixFactorPredictor(predictorName, value, p.categories.get.valueToIndex, p.matrix.get)
    } else {
      new FactorPredictor(predictorName, value)
    }
  } else {
    new CovariatePredictor(predictorName, Utils.toDouble(value))
  }
}

object PCovMatrixType extends Enumeration {
  type PCovMatrixType = Value
  val model, robust = Value
}

/**
 * Matrix of Parameter estimate covariances. Made up of PCovCells, each of them being located via row information for
 * Parameter name (pRow), row information for target variable value (tRow), column information for Parameter name (pCol)
 * and column information for target variable value (tCol). Note that the matrix is symmetric with respect to the main
 * diagonal (interchanging tRow and tCol together with pRow and pCol will not change the value). Therefore it is
 * sufficient that only half of the matrix be exported. Attributes tRow and tCol are optional since they are not needed
 * for linear regression models. This element has an optional attribute type that can take values model and robust.
 * This attribute describes the way the covariance matrix was computed in generalizedLinear model. The robust option is
 * also known as Huber-White or sandwich or HCCM.
 */
class PCovMatrix(val cells: Array[PCovCell], val tpe: Option[PCovMatrixType]) extends PmmlElement

class PCovCell(val pRow: String,
               val pCol: String,
               val value: Double,
               val tRow: Option[String] = None,
               val tCol: Option[String] = None,
               val targetCategory: Option[DataVal] = None) extends PmmlElement

/**
 * Parameter matrix. A table containing the Parameter values along with associated statistics (degrees of freedom). One
 * dimension has the target variable's categories, the other has the Parameter names. The table is represented by
 * specifying each cell. There is no requirement for Parameter names other than that each name should uniquely identify
 * one Parameter.
 */
class ParamMatrix(val cells: Array[PCell]) extends PmmlElement

/**
 * Cell in the ParamMatrix. The optional targetCategory and required parameterName attributes determine the cell's
 * location in the Parameter matrix. The information contained is: beta (actual Parameter value, required), and df
 * (degrees of freedom, optional). For ordinalMultinomial model ParamMatrix specifies different values for the
 * intercept parameter: one for each target category except one. Values for all other parameters are constant across
 * all target variable values. For multinomialLogistic model ParamMatrix specifies parameter estimates for each target
 * category except the reference category.
 */
class PCell(val parameterName: String, val beta: Double, val targetCategory: Option[DataVal], val df: Option[Int]) extends PmmlElement

class EventValues(val values: Array[Value], val intervals: Array[Interval]) extends PmmlElement

class BaseCumHazardTables(val baselineStratums: Array[BaselineStratum], val baselineCells: Array[BaselineCell], val maxTime: Option[Double]) extends PmmlElement {
  private lazy val valueToBaselineStratum: Map[DataVal, BaselineStratum] = baselineStratums.map(x => (x.value, x)).toMap

  def getBaselineStratum(value: DataVal): Option[BaselineStratum] = valueToBaselineStratum.get(value)
}

class BaselineStratum(val cells: Array[BaselineCell], val value: DataVal, val maxTime: Double, val label: Option[String]) extends PmmlElement

class BaselineCell(val time: Double, val cumHazard: Double) extends PmmlElement

/**
 * Specifies the type of regression model in use. This information will be used to select the appropriate mathematical
 * formulas during scoring.
 */
object GeneralModelType extends Enumeration {
  type GeneralModelType = Value
  val regression, generalLinear, multinomialLogistic, ordinalMultinomial, generalizedLinear, CoxRegression = Value
}

/**
 * Definition is used for specifies the type of link function to use when generalizedLinear model type is specified.
 */
object LinkFunction extends Enumeration {
  type LinkFunction = Value
  val cloglog, identity, log, logc, logit, loglog, negbin, oddspower, power, probit = Value
}

/**
 * Definition is used for specifying a cumulative link function used in ordinalMultinomial model.
 */
object CumulativeLinkFunction extends Enumeration {
  type CumulativeLinkFunction = Value
  val logit, probit, cloglog, loglog, cauchit = Value
}

/**
 * The probability distribution of the dependent variable for generalizedLinear model.
 */
object Distribution extends Enumeration {
  type Distribution = Value
  val binomial, gamma, igauss, negbin, normal, poisson, tweedie = Value
}

trait HasGeneralRegressionAttributes extends HasModelAttributes {

  /**
   * Name of the target variable (also called response variable). This attribute has been deprecated since PMML 3.0.
   * If present, it should match the name of the target MiningField.
   */
  @PmmlDeprecated(since = "3.0")
  def targetVariableName: Option[String]

  /** Specifies the type of regression model in use. */
  def modelType: GeneralModelType

  /**
   * Used for specifying the reference category of the target variable in a multinomial classification model. Normally
   * the reference category is the one from DataDictionary that does not appear in the ParamMatrix, but when several
   * models are combined in one PMML file an explicit specification is needed.
   */
  def targetReferenceCategory: Option[DataVal]

  /** Specifies the type of cumulative link function to use when ordinalMultinomial model type is specified. */
  def cumulativeLink: Option[CumulativeLinkFunction]

  /** Specifies the type of link function to use when generalizedLinear model type is specified. */
  def linkFunction: Option[LinkFunction]

  /** Specifies an additional number the following link functions need: oddspower and power. */
  def linkParameter: Option[Double]

  /**
   * Specifies an additional variable used during scoring some generalizedLinear models (see the description of scoring
   * procedure below). This attribute must refer to a DataField or a DerivedField. This attribute can only be used when
   * the distribution is binomial.
   */
  def trialsVariable: Option[Field]

  /**
   * A positive integer used during scoring some generalizedLinear models (see the description of scoring procedure
   * below). At most one of the attributes trialsVariable and trialsValue can be present in a model. This attribute can
   * only be used when the distribution is binomial.
   */
  def trialsValue: Option[Int]

  /**
   * The probability distribution of the dependent variable for generalizedLinear model may be specified as normal,
   * binomial, gamma, inverse Gaussian, negative binomial, or Poisson. Note that binomial distribution can be used in
   * two situations: either the target is categorical with two categories or a trialsVariable or trialsValue is
   * specified.
   */
  def distribution: Option[Distribution]

  /** Specifies an ancillary parameter value for the negative binomial distribution. */
  def distParameter: Option[Double]

  /**
   * If present, this variable is used during scoring generalizedLinear, ordinalMultinomial, or multinomialLogistic
   * models (see the description of scoring procedures below). This attribute must refer to a DataField or a
   * DerivedField.
   */
  def offsetVariable: Option[Field]

  /**
   * If present, this value is used during scoring generalizedLinear, ordinalMultinomial, or multinomialLogistic models.
   * It works like a user-specified intercept (see the description of the scoring procedures below). At most one of the
   * attributes offsetVariable and offsetValue can be present in a model.
   */
  def offsetValue: Option[Double]

  /**
   * The value of degrees of freedom for the model. This value is needed for computing confidence intervals for
   * predicted values.
   */
  def modelDF: Option[Double]

  /**
   * If modelType is CoxRegression, this variable is required during scoring (see the description of scoring procedures
   * below). This attribute must refer to a DataField or a DerivedField containing a continuous variable.
   */
  def endTimeVariable: Option[Field]

  /**
   * If modelType is CoxRegression, this variable is optional, it is not used during scoring but is an important piece
   * of information about model building. This attribute must refer to a DataField or a DerivedField containing a
   * continuous variable.
   */
  def startTimeVariable: Option[Field]

  /**
   * If modelType is CoxRegression, this variable is optional, it is not used during scoring but is an important piece
   * of information about model building. This attribute must refer to a DataField or a DerivedField. Explicitly
   * listing all categories of this variable is not recommended.
   */
  def subjectIDVariable: Option[Field]

  /**
   * If modelType is CoxRegression, this variable is optional. This attribute must refer to a DataField or a
   * DerivedField.
   */
  def statusVariable: Option[Field]

  /**
   * If modelType is CoxRegression, this variable is optional, if present it is used during scoring (see the
   * description of scoring procedures below). This attribute must refer to a DataField or a DerivedField containing a
   * categorical variable.
   */
  def baselineStrataVariable: Option[Field]
}

trait HasWrappedGeneralRegressionAttributes extends HasWrappedModelAttributes with HasGeneralRegressionAttributes {

  override def attributes: GeneralRegressionAttributes

  override def targetVariableName: Option[String] = attributes.targetVariableName

  override def modelType: GeneralModelType = attributes.modelType

  override def cumulativeLink: Option[CumulativeLinkFunction] = attributes.cumulativeLink

  override def linkFunction: Option[LinkFunction] = attributes.linkFunction

  override def linkParameter: Option[Double] = attributes.linkParameter

  override def trialsVariable: Option[Field] = attributes.trialsVariable

  override def trialsValue: Option[Int] = attributes.trialsValue

  override def distribution: Option[Distribution] = attributes.distribution

  override def distParameter: Option[Double] = attributes.distParameter

  override def offsetVariable: Option[Field] = attributes.offsetVariable

  override def offsetValue: Option[Double] = attributes.offsetValue

  override def modelDF: Option[Double] = attributes.modelDF

  override def endTimeVariable: Option[Field] = attributes.endTimeVariable

  override def startTimeVariable: Option[Field] = attributes.startTimeVariable

  override def subjectIDVariable: Option[Field] = attributes.subjectIDVariable

  override def statusVariable: Option[Field] = attributes.statusVariable

  override def baselineStrataVariable: Option[Field] = attributes.baselineStrataVariable
}

class GeneralRegressionAttributes(
                                   override val functionName: MiningFunction,
                                   val modelType: GeneralModelType,
                                   val targetVariableName: Option[String] = None,
                                   val targetReferenceCategory: Option[String] = None,
                                   val cumulativeLink: Option[CumulativeLinkFunction] = None,
                                   val linkFunction: Option[LinkFunction] = None,
                                   val linkParameter: Option[Double] = None,
                                   val trialsVariable: Option[Field] = None,
                                   val trialsValue: Option[Int] = None,
                                   val distribution: Option[Distribution] = None,
                                   val distParameter: Option[Double] = None,
                                   val offsetVariable: Option[Field] = None,
                                   val offsetValue: Option[Double] = None,
                                   val modelDF: Option[Double] = None,
                                   val endTimeVariable: Option[Field] = None,
                                   val startTimeVariable: Option[Field] = None,
                                   val subjectIDVariable: Option[Field] = None,
                                   val statusVariable: Option[Field] = None,
                                   val baselineStrataVariable: Option[Field] = None,
                                   override val modelName: Option[String] = None,
                                   override val algorithmName: Option[String] = None,
                                   override val isScorable: Boolean = true)
  extends ModelAttributes(functionName, modelName, algorithmName, isScorable)

class GeneralRegressionOutputs extends MixedClsWithRegOutputs {
  override def modelElement: ModelElement = ModelElement.GeneralRegressionModel
}


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
package org.pmml4s.xml

import org.pmml4s.common.{Interval, Matrix, Value}
import org.pmml4s.model._

/**
 * Builder of General Regression Model
 */
class GeneralRegressionBuilder extends Builder[GeneralRegressionModel] {
  protected var attributes: GeneralRegressionAttributes = _
  private var parameterList: ParameterList = _
  private var factorList: FactorList = _
  private var covariateList: CovariateList = _
  private var ppMatrix: PPMatrix = _
  private var pCovMatrix: PCovMatrix = _
  private var paramMatrix: ParamMatrix = _
  private var eventValues: EventValues = _
  private var baseCumHazardTables: BaseCumHazardTables = _

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): GeneralRegressionModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.GENERAL_REGRESSION_MODEL, {
      case EvElemStart(_, ElemTags.PARAMETER_LIST, _, _)             => parameterList = makeParameterList(reader)
      case EvElemStart(_, ElemTags.FACTOR_LIST, _, _)                => factorList = makeFactorList(reader)
      case EvElemStart(_, ElemTags.COVARIATE_LIST, _, _)             => covariateList = makeCovariateList(reader)
      case EvElemStart(_, ElemTags.PP_MATRIX, _, _)                  => ppMatrix = makePPMatrix(reader)
      case EvElemStart(_, ElemTags.P_COV_MATRIX, attrs, _)           => pCovMatrix = makePCovMatrix(reader, attrs)
      case EvElemStart(_, ElemTags.PARAM_MATRIX, _, _)               => paramMatrix = makeParamMatrix(reader)
      case EvElemStart(_, ElemTags.EVENT_VALUES, _, _)               => eventValues = makeEventValues(reader)
      case EvElemStart(_, ElemTags.BASE_CUM_HAZARD_TABLES, attrs, _) =>
        baseCumHazardTables = makeBaseCumHazardTables(reader, attrs)
    })

    new GeneralRegressionModel(parent, attributes, miningSchema,
      parameterList, Option(factorList), Option(covariateList), ppMatrix, Option(pCovMatrix), paramMatrix, Option(eventValues), Option(baseCumHazardTables),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeBaselineCell(reader: XMLEventReader, attrs: XmlAttrs): BaselineCell = {
    val time = attrs.double(AttrTags.TIME)
    val cumHazard = attrs.double(AttrTags.CUM_HAZARD)

    new BaselineCell(time, cumHazard)
  }

  def makeBaseCumHazardTables(reader: XMLEventReader, attrs: XmlAttrs): BaseCumHazardTables = makeElem(reader, attrs,
    new ElemBuilder[BaseCumHazardTables] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): BaseCumHazardTables = {
        val (baselineStratums, baselineCells) = makeElems(reader, ElemTags.BASE_CUM_HAZARD_TABLES,
          ElemTags.BASELINE_STRATUM, new ElemBuilder[BaselineStratum] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): BaselineStratum = {
              val value = attrs(AttrTags.VALUE)
              val label = attrs.get(AttrTags.LABEL)
              val maxTime = attrs.double(AttrTags.MAX_TIME)
              val cells = makeElems(reader, ElemTags.BASELINE_STRATUM, ElemTags.BASELINE_CELL, new ElemBuilder[BaselineCell] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): BaselineCell = makeBaselineCell(reader, attrs)
              })

              new BaselineStratum(cells, attributes.baselineStrataVariable.get.toVal(value), maxTime, label)
            }
          }, ElemTags.BASELINE_CELL, new ElemBuilder[BaselineCell] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): BaselineCell = makeBaselineCell(reader, attrs)
          })
        val maxTime: Option[Double] = attrs.getDouble(AttrTags.MAX_TIME)

        new BaseCumHazardTables(baselineStratums, baselineCells, maxTime)
      }
    })

  def makeEventValues(reader: XMLEventReader): EventValues = {
    val (values, intervals) = makeElems(reader, ElemTags.EVENT_VALUES, ElemTags.VALUE, new ElemBuilder[Value] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Value = makeValue(reader, attrs)
    }, ElemTags.INTERVAL, new ElemBuilder[Interval] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Interval = makeInterval(reader, attrs)
    })

    new EventValues(values, intervals)
  }

  def makeParamMatrix(reader: XMLEventReader): ParamMatrix = {
    val cells = makeElems(reader, ElemTags.PARAM_MATRIX, ElemTags.P_CELL, new ElemBuilder[PCell] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): PCell = {
        // 'targetCategory' is required for the classification model, while useless for the regression model. Some
        // examples still write an empty string for regression, so just ignore it
        val targetCategory = if (attributes.isRegression) None else attrs.get(AttrTags.TARGET_CATEGORY).map(target.toVal(_))
        val parameterName = attrs(AttrTags.PARAMETER_NAME)

        // 'beta' is a required attribute defined by the PMML standard, but some examples exported from R could be absent.
        val beta = attrs.getDouble(AttrTags.BETA, 0.0)
        val df = attrs.getInt(AttrTags.DF)

        new PCell(parameterName, beta, targetCategory, df)
      }
    })

    new ParamMatrix(cells)
  }

  def makePCovMatrix(reader: XMLEventReader, attrs: XmlAttrs): PCovMatrix = makeElem(reader, attrs, new ElemBuilder[PCovMatrix] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): PCovMatrix = {
      val tpe = attrs.getEnum(AttrTags.TYPE, PCovMatrixType)
      val cells = makeElems(reader, ElemTags.P_COV_MATRIX, ElemTags.P_COV_CELL, new ElemBuilder[PCovCell] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): PCovCell = {
          val pRow = attrs(AttrTags.P_ROW)
          val pCol = attrs(AttrTags.P_COL)
          val tRow = attrs.get(AttrTags.T_ROW)
          val tCol = attrs.get(AttrTags.T_COL)
          val value = attrs.double(AttrTags.VALUE)
          val targetCategory = attrs.get(AttrTags.TARGET_CATEGORY).map(target.toVal(_))

          new PCovCell(pRow, pCol, value, tRow, tCol, targetCategory)
        }
      })

      new PCovMatrix(cells, tpe)
    }
  })

  def makePPMatrix(reader: XMLEventReader): PPMatrix = {
    val cells = makeElems(reader, ElemTags.PP_MATRIX, ElemTags.PP_CELL, new ElemBuilder[PPCell] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): PPCell = {
        val predictorName = attrs(AttrTags.PREDICTOR_NAME)
        val f = field(predictorName)
        val value = f.toVal(attrs(AttrTags.VALUE))
        val parameterName = attrs(AttrTags.PARAMETER_NAME)
        val targetCategory = attrs.get(AttrTags.TARGET_CATEGORY).map(target.toVal)

        new PPCell(value, f, parameterName, targetCategory)
      }
    })
    new PPMatrix(cells)
  }

  def makeCovariateList(reader: XMLEventReader): CovariateList = {
    val predictors = makeElems(reader, ElemTags.COVARIATE_LIST, ElemTags.PREDICTOR, new ElemBuilder[Predictor] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Predictor = makePredictor(reader, attrs)
    })

    new CovariateList(predictors)
  }

  def makePredictor(reader: XMLEventReader, attrs: XmlAttrs): Predictor = {
    val name = attrs(AttrTags.NAME)
    val f = field(name)
    val contrastMatrixType = attrs.get((AttrTags.CONTRAST_MATRIX_TYPE))
    var categories: Categories = null
    var matrix: Matrix = null
    traverseElems(reader, ElemTags.PREDICTOR, {
      case EvElemStart(_, ElemTags.CATEGORIES, attrs, _) => categories = makeElem(reader, attrs, new ElemBuilder[Categories] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Categories = {
          val categories = makeElems(reader, ElemTags.CATEGORIES, ElemTags.CATEGORY, new ElemBuilder[Category] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): Category = {
              val value = f.toVal(attrs(AttrTags.VALUE))
              new Category(value)
            }
          })
          new Categories(categories)
        }
      })
      case EvElemStart(_, ElemTags.MATRIX, attrs, _)     => matrix = makeMatrix(reader, attrs)
    })

    new Predictor(name, contrastMatrixType, Option(categories), Option(matrix))
  }

  def makeFactorList(reader: XMLEventReader): FactorList = {
    val predictors = makeElems(reader, ElemTags.FACTOR_LIST, ElemTags.PREDICTOR, new ElemBuilder[Predictor] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Predictor = makePredictor(reader, attrs)
    })

    new FactorList(predictors)
  }

  def makeParameterList(reader: XMLEventReader): ParameterList = {
    val parameters = makeElems(reader, ElemTags.PARAMETER_LIST, ElemTags.PARAMETER, new ElemBuilder[Parameter] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): Parameter = {
        val name = attrs(AttrTags.NAME)
        val label = attrs.get(AttrTags.LABEL)
        val referencePoint = attrs.getDouble(AttrTags.REFERENCE_POINT).getOrElse(0.0)

        new Parameter(name, label, referencePoint)
      }
    })

    new ParameterList(parameters)
  }


  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): GeneralRegressionAttributes = {
    val attributes = super.makeAttributes(attrs)

    new GeneralRegressionAttributes(
      functionName = attributes.functionName,
      modelType = attrs.`enum`(AttrTags.MODEL_TYPE, GeneralModelType),
      targetVariableName = attrs.get(AttrTags.TARGET_VARIABLE_NAME),
      targetReferenceCategory = attrs.get(AttrTags.TARGET_REFERENCE_CATEGORY),
      cumulativeLink = attrs.getEnum(AttrTags.CUMULATIVE_LINK, CumulativeLinkFunction),
      linkFunction = attrs.getEnum(AttrTags.LINK_FUNCTION, LinkFunction),
      linkParameter = attrs.getDouble(AttrTags.LINK_PARAMETER),
      trialsVariable = attrs.get(AttrTags.TRIALS_VARIABLE).map(field(_)),
      trialsValue = attrs.getInt(AttrTags.TRIALS_VALUE),
      distribution = attrs.getEnum(AttrTags.DISTRIBUTION, Distribution),
      distParameter = attrs.getDouble(AttrTags.DIST_PARAMETER),
      offsetVariable = attrs.get(AttrTags.OFFSET_VARIABLE).map(field(_)),
      offsetValue = attrs.getDouble(AttrTags.OFFSET_VALUE),
      modelDF = attrs.getDouble(AttrTags.MODEL_DF),
      endTimeVariable = attrs.get(AttrTags.END_TIME_VARIABLE).map(field(_)),
      startTimeVariable = attrs.get(AttrTags.START_TIME_VARIABLE).map(field(_)),
      subjectIDVariable = attrs.get(AttrTags.SUBJECT_ID_VARIABLE).map(field(_)),
      statusVariable = attrs.get(AttrTags.STATUS_VARIABLE).map(field(_)),
      baselineStrataVariable = attrs.get(AttrTags.BASELINE_STRATA_VARIABLE).map(field(_)),
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable)
  }

  /** Name of the builder. */
  override def name: String = ElemTags.GENERAL_REGRESSION_MODEL
}


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

import org.pmml4s.common.{NumericPredictor, PredictorTerm, RegressionPredictor, RegressionTable}
import org.pmml4s.data.DataVal
import org.pmml4s.model._
import org.pmml4s.transformations.FieldRef

import scala.collection.mutable

/**
 * Builder of Regression model.
 */
class RegressionBuilder extends Builder[RegressionModel] {
  protected var attributes: RegressionAttributes = _
  private val regressionTables = mutable.ArrayBuilder.make[RegressionTable]

  /** Builds a Regression model from a specified XML reader, which points the element <RegressionModel> */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): RegressionModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.REGRESSION_MODEL, {
      case EvElemStart(_, ElemTags.REGRESSION_TABLE, attrs, _) => regressionTables += makeRegressionTable(reader, attrs)
    })

    new RegressionModel(parent, attributes, miningSchema, regressionTables.result(),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  private def makeRegressionTable(reader: XMLEventReader, attrs: XmlAttrs): RegressionTable = makeElem(reader, attrs, new ElemBuilder[RegressionTable] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): RegressionTable = {
      val intercept = attrs.double(AttrTags.INTERCEPT)
      val targetCategory = attrs.get(AttrTags.TARGET_CATEGORY).map(x => {
        val t = rootTarget
        if (t != null && t.isCategorical) {
          t.toVal(x)
        } else {
          DataVal.from(x)
        }
      })
      val predictors = makeElems(reader, ElemTags.REGRESSION_TABLE, RegressionPredictor.values, new GroupElemBuilder[RegressionPredictor] {
        def build(reader: XMLEventReader, event: EvElemStart): RegressionPredictor = makePredictor(reader, event)
      })

      new RegressionTable(predictors, intercept, targetCategory)
    }
  })

  private def makePredictor(reader: XMLEventReader, event: EvElemStart): RegressionPredictor = makeElem(reader, event, new GroupElemBuilder[RegressionPredictor] {
    def build(reader: XMLEventReader, event: EvElemStart): RegressionPredictor = event match {
      case EvElemStart(_, ElemTags.NUMERIC_PREDICTOR, attrs, _)     => makeElem(reader, attrs, new ElemBuilder[NumericPredictor] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): NumericPredictor = {
          val name = attrs(AttrTags.NAME)
          val exponent = attrs.getInt(AttrTags.EXPONENT, 1)
          val coefficient = attrs.double(AttrTags.COEFFICIENT)

          new NumericPredictor(field(name), coefficient, exponent)
        }
      })
      case EvElemStart(_, ElemTags.CATEGORICAL_PREDICTOR, attrs, _) => makeCategoricalPredictor(reader, attrs)
      case EvElemStart(_, ElemTags.PREDICTOR_TERM, attrs, _)        => makeElem(reader, attrs, new ElemBuilder[RegressionPredictor] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): RegressionPredictor = {
          val name = attrs.get(AttrTags.NAME)
          val coefficient = attrs.double(AttrTags.COEFFICIENT)
          val fields = makeElems(reader, ElemTags.PREDICTOR_TERM, ElemTags.FIELD_REF, new ElemBuilder[FieldRef] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): FieldRef = {
              val f = field(attrs(AttrTags.FIELD))
              val mapMissingTo = attrs.get(AttrTags.MAP_MISSING_TO).map(f.toVal)
              new FieldRef(f, mapMissingTo)
            }
          })

          new PredictorTerm(name, coefficient, fields)
        }
      })
      case _                                                        => ??????
    }
  })

  override def makeAttributes(attrs: XmlAttrs): RegressionAttributes = {
    val attributes = super.makeAttributes(attrs)

    new RegressionAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      modelType = attrs.get(AttrTags.MODEL_TYPE) map { x => RegressionModelType.withName(x) },
      targetFieldName = attrs.get(AttrTags.TARGET_FIELD_NAME),
      normalizationMethod = attrs.get(AttrTags.NORMALIZATION_METHOD) map { x => RegressionNormalizationMethod.withName(x) } getOrElse RegressionNormalizationMethod.none)
  }

  /** Name of the builder */
  override def name: String = ElemTags.REGRESSION_MODEL
}


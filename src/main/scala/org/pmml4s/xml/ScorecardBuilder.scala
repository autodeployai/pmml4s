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

import org.pmml4s.common.Predicate
import org.pmml4s.model._
import org.pmml4s.transformations.Expression

import scala.collection.mutable

/**
 * Builder of Scorecard model
 */
class ScorecardBuilder extends Builder[Scorecard] {
  protected var attributes: ScorecardAttributes = _
  private var characteristics: Characteristics = _

  /** Builds a Scorecard model from a specified XML reader, which points the element <Scorecard> */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): Scorecard = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.SCORECARD, {
      case EvElemStart(_, ElemTags.CHARACTERISTICS, attrs, _) => characteristics = makeCharacteristics(reader, attrs)
    })

    new Scorecard(parent, attributes, miningSchema, characteristics,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  /** Parses the Characteristics node */
  private def makeCharacteristics(reader: XMLEventReader, attrs: XmlAttrs): Characteristics = makeElem(reader, attrs, new ElemBuilder[Characteristics] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): Characteristics = {
      val characteristics = mutable.ArrayBuilder.make[Characteristic]

      traverseElems(reader, ElemTags.CHARACTERISTICS, {
        case EvElemStart(_, ElemTags.CHARACTERISTIC, attrs, _) => characteristics += makeCharacteristic(reader, attrs)
        case _                                                 =>
      })

      new Characteristics(characteristics.result())
    }
  })

  /** Parses the Characteristic node */
  private def makeCharacteristic(reader: XMLEventReader, attrs: XmlAttrs): Characteristic = makeElem(reader, attrs, new ElemBuilder[Characteristic] {
    def build(reader: XMLEventReader, attrs: XmlAttrs): Characteristic = {
      val name: Option[String] = attrs.get(AttrTags.NAME)
      val reasonCode: Option[String] = attrs.get(AttrTags.REASON_CODE)
      val baselineScore: Option[Double] = attrs.getDouble(AttrTags.BASELINE_SCORE)
      val attributes = mutable.ArrayBuilder.make[Attribute]

      traverseElems(reader, ElemTags.CHARACTERISTIC, {
        case EvElemStart(_, ElemTags.ATTRIBUTE, attrs, _) => attributes += makeAttribute(reader, attrs)
        case _                                            =>
      })

      new Characteristic(name, reasonCode, baselineScore, attributes.result())
    }
  })

  /** Parses the Attribute node */
  private def makeAttribute(reader: XMLEventReader, attrs: XmlAttrs): Attribute = makeElem(reader, attrs, new
      ElemBuilder[Attribute] {

    override def build(reader: XMLEventReader, attrs: XmlAttrs): Attribute = {
      val reasonCode: Option[String] = attrs.get(AttrTags.REASON_CODE)
      val partialScore: Option[Double] = attrs.getDouble(AttrTags.PARTIAL_SCORE)

      var predicate: Predicate = null
      var complexPartialScore: ComplexPartialScore = null
      traverseElems(reader, ElemTags.ATTRIBUTE, {
        case event: EvElemStart if Predicate.contains(event.label)    => predicate = makePredicate(reader, event)
        case EvElemStart(_, ElemTags.COMPLEX_PARTIAL_SCORE, attrs, _) =>
          complexPartialScore = makeComplexPartialScore(reader, attrs)
        case _                                                        =>
      })

      new Attribute(reasonCode, partialScore, predicate, Option(complexPartialScore))
    }
  })

  /** Parses the ComplexPartialScore node */
  private def makeComplexPartialScore(reader: XMLEventReader, attrs: XmlAttrs): ComplexPartialScore =
    makeElem(reader, attrs, new ElemBuilder[ComplexPartialScore] {

      override def build(reader: XMLEventReader, attrs: XmlAttrs): ComplexPartialScore = {
        var expression: Expression = null
        traverseElems(reader, ElemTags.COMPLEX_PARTIAL_SCORE, {
          case event: EvElemStart if Expression.contains(event.label) =>
            expression = makeExpression(reader, event, defaultFieldScope)
          case _                                                      =>
        })

        new ComplexPartialScore(expression)
      }
    })

  override def makeAttributes(attrs: XmlAttrs): ScorecardAttributes = {
    val attributes = super.makeAttributes(attrs)

    new ScorecardAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      initialScore = attrs.getDouble(AttrTags.INITIAL_SCORE, 0.0),
      useReasonCodes = attrs.getBoolean(AttrTags.USE_REASON_CODES, true),
      reasonCodeAlgorithm = attrs.get(AttrTags.REASON_CODE_ALGORITHM) map { x => ReasonCodeAlgorithm.withName(x) }
        getOrElse ReasonCodeAlgorithm.pointsBelow,
      baselineScore = attrs.getDouble(AttrTags.BASELINE_SCORE),
      baselineMethod = attrs.get(AttrTags.BASELINE_METHOD) map { x => BaselineMethod.withName(x) }
        getOrElse BaselineMethod.other)
  }

  /** Name of the builder. */
  def name: String = ElemTags.SCORECARD
}

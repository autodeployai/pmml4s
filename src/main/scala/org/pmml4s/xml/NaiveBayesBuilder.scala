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

import org.pmml4s.common.ContinuousDistribution
import org.pmml4s.metadata.Field
import org.pmml4s.model._
import org.pmml4s.transformations.DerivedField

import scala.collection.mutable.ArrayBuffer

/**
 * Builder of Naive Bayes Model
 */
class NaiveBayesBuilder extends Builder[NaiveBayesModel] {
  protected var attributes: NaiveBayesAttributes = _
  private var bayesInputs: BayesInputs = _
  private var bayesOutput: BayesOutput = _

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): NaiveBayesModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.NAIVE_BAYES_MODEL, {
      case EvElemStart(_, ElemTags.BAYES_INPUTS, _, _)     => bayesInputs = makeBayesInputs(reader)
      case EvElemStart(_, ElemTags.BAYES_OUTPUT, attrs, _) => bayesOutput = makeBayesOutput(reader, attrs)
    })

    new NaiveBayesModel(parent, attributes, miningSchema,
      bayesInputs, bayesOutput,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  private def makeBayesInputs(reader: XMLEventReader): BayesInputs = {
    val inputs = makeElems(reader, ElemTags.BAYES_INPUTS, ElemTags.BAYES_INPUT,
      new ElemBuilder[BayesInput] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): BayesInput = {
          val fieldName = attrs(AttrTags.FIELD_NAME)
          val f = field(fieldName)
          var targetValueStats: Option[TargetValueStats] = None
          val pairCounts = new ArrayBuffer[PairCounts]()
          var derivedField: Option[DerivedField] = None
          traverseElems(reader, ElemTags.BAYES_INPUT, {
            case EvElemStart(_, ElemTags.TARGET_VALUE_STATS, _, _) => targetValueStats = {
              val targetValueStats = makeElems(reader, ElemTags.TARGET_VALUE_STATS, ElemTags.TARGET_VALUE_STAT,
                new ElemBuilder[TargetValueStat] {
                  override def build(reader: XMLEventReader, attrs: XmlAttrs): TargetValueStat = {
                    val value = verifyValue(attrs(AttrTags.VALUE), target)
                    var distribution: ContinuousDistribution = null
                    traverseElems(reader, ElemTags.TARGET_VALUE_STAT, {
                      case event: EvElemStart if ContinuousDistribution.contains(event.label) =>
                        distribution = makeContinuousDistribution(reader, event)
                      case _                                                                  =>
                    })

                    new TargetValueStat(value, distribution)
                  }
                })

              Some(new TargetValueStats(targetValueStats))
            }
            case EvElemStart(_, ElemTags.DERIVED_FIELD, attrs, _)  => derivedField = Option(makeDerivedField(reader, attrs))
            case EvElemStart(_, ElemTags.PAIR_COUNTS, attrs, _)    => pairCounts += makeElem(reader, attrs, new ElemBuilder[PairCounts] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): PairCounts = {
                val af: Field = derivedField.getOrElse(f)
                val value = verifyValue(attrs(AttrTags.VALUE), af)
                val targetValueCounts = makeElem(reader, ElemTags.PAIR_COUNTS, ElemTags.TARGET_VALUE_COUNTS,
                  new ElemBuilder[TargetValueCounts] {
                    override def build(reader: XMLEventReader, attrs: XmlAttrs): TargetValueCounts = makeTargetValueCounts(reader)
                  })
                new PairCounts(value, targetValueCounts.get)
              }
            })
            case _                                                 =>
          })

          new BayesInput(f, targetValueStats, pairCounts.toArray, derivedField)
        }
      })

    new BayesInputs(inputs)
  }

  private def makeBayesOutput(reader: XMLEventReader, attrs: XmlAttrs): BayesOutput = makeElem(reader, attrs,
    new ElemBuilder[BayesOutput] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): BayesOutput = {
        val fieldName = attrs(AttrTags.FIELD_NAME)
        val f = field(fieldName)
        val targetValueCounts = makeElem(reader, ElemTags.BAYES_OUTPUT, ElemTags.TARGET_VALUE_COUNTS,
          new ElemBuilder[TargetValueCounts] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): TargetValueCounts = makeTargetValueCounts(reader)
          })

        new BayesOutput(f, targetValueCounts.get)
      }
    })

  private def makeTargetValueCounts(reader: XMLEventReader): TargetValueCounts = {
    val targetValueCounts = makeElems(reader, ElemTags.TARGET_VALUE_COUNTS, ElemTags.TARGET_VALUE_COUNT,
      new ElemBuilder[TargetValueCount] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): TargetValueCount = {
          val value = verifyValue(attrs(AttrTags.VALUE), target)
          val count = attrs.double(AttrTags.COUNT)

          new TargetValueCount(value, count)
        }
      })

    new TargetValueCounts(targetValueCounts)
  }

  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): NaiveBayesAttributes = {
    val attributes = super.makeAttributes(attrs)

    new NaiveBayesAttributes(
      threshold = attrs.double(AttrTags.THRESHOLD),
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable)
  }

  /** Name of the builder. */
  override def name: String = ElemTags.NAIVE_BAYES_MODEL
}


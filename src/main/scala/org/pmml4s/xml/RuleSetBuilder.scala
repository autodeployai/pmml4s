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
package org.pmml4s.xml

import org.pmml4s.common.{ModelAttributes, Predicate, ScoreDistribution, ScoreDistributions}
import org.pmml4s.model._

import scala.collection.mutable
import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Builder of Rule Set model.
 */
class RuleSetBuilder extends Builder[RuleSetModel] {
  protected var attributes: ModelAttributes = _
  private var ruleSet: RuleSet = _

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): RuleSetModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.RULE_SET_MODEL, {
      case EvElemStart(_, ElemTags.RULE_SET, attrs, _) => ruleSet = makeRuleSet(reader, attrs)
    })

    new RuleSetModel(parent, attributes, miningSchema, ruleSet,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeRuleSet(reader: XMLEventReader, attrs: MetaData): RuleSet = makeElem(reader, attrs, new ElemBuilder[RuleSet] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): RuleSet = {
      val recordCount = attrs.getInt(AttrTags.RECORD_COUNT)
      val nbCorrect = attrs.getInt(AttrTags.NB_CORRECT)
      val defaultScore = attrs.get(AttrTags.DEFAULT_SCORE).map(verifyScore(_))
      val defaultConfidence = attrs.getDouble(AttrTags.DEFAULT_CONFIDENCE)
      val ruleSelectionMethods = mutable.ArrayBuilder.make[RuleSelectionMethod]()
      val scoreDistributions = mutable.ArrayBuilder.make[ScoreDistribution]()
      val rules = mutable.ArrayBuilder.make[Rule]()

      traverseElems(reader, ElemTags.RULE_SET, {
        case EvElemStart(_, ElemTags.RULE_SELECTION_METHOD, attrs, _) => ruleSelectionMethods += makeRuleSelectionMethod(reader, attrs)
        case EvElemStart(_, ElemTags.SCORE_DISTRIBUTION, attrs, _)    => scoreDistributions += makeScoreDistribution(reader, attrs)
        case event: EvElemStart if Rule.contains(event.label)         => rules += makeRule(reader, event)
      })

      new RuleSet(ruleSelectionMethods.result(), new ScoreDistributions(scoreDistributions.result()), rules.result(),
        recordCount, nbCorrect, defaultScore, defaultConfidence)
    }
  })

  def makeRule(reader: XMLEventReader, event: EvElemStart): Rule = makeElem(reader, event, new GroupElemBuilder[Rule] {
    override def build(reader: XMLEventReader, event: EvElemStart): Rule = event match {
      case EvElemStart(_, ElemTags.SIMPLE_RULE, attrs, _)   => makeElem(reader, attrs, new ElemBuilder[SimpleRule] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): SimpleRule = {
          val id = attrs.get(AttrTags.ID)
          val score = verifyScore(attrs(AttrTags.SCORE))
          val recordCount = attrs.getInt(AttrTags.RECORD_COUNT)
          val nbCorrect = attrs.getInt(AttrTags.NB_CORRECT)
          val confidence = attrs.getDouble(AttrTags.CONFIDENCE, 1.0)
          val weight = attrs.getDouble(AttrTags.WEIGHT, 1.0)
          var predicate: Predicate = null
          val scoreDistributions = mutable.ArrayBuilder.make[ScoreDistribution]()
          traverseElems(reader, ElemTags.SIMPLE_RULE, {
            case event: EvElemStart if Predicate.contains(event.label) => predicate = makePredicate(reader, event)
            case EvElemStart(_, ElemTags.SCORE_DISTRIBUTION, attrs, _) => scoreDistributions += makeScoreDistribution(reader, attrs)
          })

          new SimpleRule(predicate, new ScoreDistributions(scoreDistributions.result()), score, id, recordCount, nbCorrect, confidence, weight)
        }
      })
      case EvElemStart(_, ElemTags.COMPOUND_RULE, attrs, _) => makeElem(reader, attrs, new ElemBuilder[CompoundRule] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): CompoundRule = {
          val rules = mutable.ArrayBuilder.make[Rule]()
          var predicate: Predicate = null

          traverseElems(reader, ElemTags.SIMPLE_RULE, {
            case event: EvElemStart if Predicate.contains(event.label) => predicate = makePredicate(reader, event)
            case event: EvElemStart if Rule.contains(event.label)      => rules += makeRule(reader, event)
          })

          new CompoundRule(predicate, rules.result())
        }
      })
      case _                                                => ??????
    }
  })

  def makeRuleSelectionMethod(reader: XMLEventReader, attrs: MetaData): RuleSelectionMethod = makeElem(reader, attrs,
    new ElemBuilder[RuleSelectionMethod] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): RuleSelectionMethod = {
        val criterion = Criterion.withName(attrs(AttrTags.CRITERION))

        new RuleSelectionMethod(criterion)
      }
    })

  /** Name of the builder. */
  override def name: String = ElemTags.RULE_SET_MODEL
}

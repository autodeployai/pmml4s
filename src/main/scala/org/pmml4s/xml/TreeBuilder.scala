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

import org.pmml4s.common.{Predicate, ScoreDistribution, ScoreDistributions}
import org.pmml4s.model._

import scala.collection.mutable
import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Builder of Tree model
 */
class TreeBuilder extends Builder[TreeModel] {
  protected var attributes: TreeAttributes = _
  private var node: Node = _

  /** Builds a Tree model from a specified XML reader, which points the element <TreeModel> */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): TreeModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.TREE_MODEL, {
      case EvElemStart(_, ElemTags.NODE, attrs, _) => node = makeNode(reader, attrs)
    })

    new TreeModel(parent, attributes, miningSchema, node,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  /** Parses the tree node */
  private def makeNode(reader: XMLEventReader, attrs: MetaData): Node = makeElem(reader, attrs, new ElemBuilder[Node] {
    def build(reader: XMLEventReader, attrs: XmlAttrs): Node = {
      val id: Option[String] = attrs.get(AttrTags.ID)
      val score: Option[Any] = attrs.get(AttrTags.SCORE).map(verifyScore(_))
      val recordCount: Option[Double] = attrs.getDouble(AttrTags.RECORD_COUNT)
      val defaultChild: Option[String] = attrs.get(AttrTags.DEFAULT_CHILD)

      var predicate: Predicate = null
      val children = mutable.ArrayBuilder.make[Node]
      val scoreDistributes = mutable.ArrayBuilder.make[ScoreDistribution]

      traverseElems(reader, ElemTags.NODE, {
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _)          => extHandler(reader, attrs).foreach { x => extensions += x }
        case event: EvElemStart if Predicate.contains(event.label) => predicate = makePredicate(reader, event)
        case EvElemStart(_, ElemTags.SCORE_DISTRIBUTION, attrs, _) => scoreDistributes += makeScoreDistribution(reader, attrs)
        case EvElemStart(_, ElemTags.NODE, attrs, _)               => children += makeNode(reader, attrs)
        case _                                                     =>
      })

      new Node(predicate, children.result(), id, score, recordCount, defaultChild, new ScoreDistributions(scoreDistributes.result()))
    }
  })

  override def makeAttributes(attrs: XmlAttrs): TreeAttributes = {
    val attributes = super.makeAttributes(attrs)

    new TreeAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      missingValueStrategy = attrs.get(AttrTags.MISSING_VALUE_STRATEGY) map { x => MissingValueStrategy.withName(x) } getOrElse MissingValueStrategy.none,
      missingValuePenalty = attrs.getDouble(AttrTags.MISSING_VALUE_PENALTY, 1.0),
      noTrueChildStrategy = attrs.get(AttrTags.NO_TRUE_CHILD_STRATEGY) map { x => NoTrueChildStrategy.withName(x) } getOrElse NoTrueChildStrategy.returnNullPrediction,
      splitCharacteristic = attrs.get(AttrTags.SPLIT_CHARACTERISTIC) map { x => SplitCharacteristic.withName(x) } getOrElse SplitCharacteristic.multiSplit)
  }

  /** Name of the builder. */
  def name: String = ElemTags.TREE_MODEL
}
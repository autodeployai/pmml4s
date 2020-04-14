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

import org.pmml4s.model._

import scala.collection.mutable
import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Builder of Association model
 */
class AssociationBuilder extends Builder[AssociationModel] {
  protected var attributes: AssociationAttributes = _
  private val items = mutable.ArrayBuilder.make[Item]
  private val itemsets = mutable.ArrayBuilder.make[Itemset]
  private val associationRules = mutable.ArrayBuilder.make[AssociationRule]

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): AssociationModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)
    items.sizeHint(attributes.numberOfItems)
    itemsets.sizeHint(attributes.numberOfItemsets)
    associationRules.sizeHint(attributes.numberOfRules)

    traverseModel(reader, ElemTags.ASSOCIATION_MODEL, {
      case EvElemStart(_, ElemTags.ITEM, attrs, _)             => items += makeItem(reader, attrs)
      case EvElemStart(_, ElemTags.ITEMSET, attrs, _)          => itemsets += makeItemset(reader, attrs)
      case EvElemStart(_, ElemTags.ASSOCIATION_RULE, attrs, _) => associationRules += makeAssociationRule(reader, attrs)
    })

    new AssociationModel(parent, attributes, miningSchema,
      items.result(), itemsets.result(), associationRules.result(),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeItem(reader: XMLEventReader, attrs: MetaData): Item = makeElem(reader, attrs, new ElemBuilder[Item] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): Item = {
      val id = attrs(AttrTags.ID)
      val fld = attrs.get(AttrTags.FIELD).map(field(_))
      val value = fld.map(x => verifyValue(attrs(AttrTags.VALUE), x)).getOrElse(attrs(AttrTags.VALUE))
      val category = attrs.get(AttrTags.CATEGORY)
      val mappedValue = attrs.get(AttrTags.MAPPED_VALUE)
      val weight = attrs.getDouble(AttrTags.WEIGHT)

      new Item(id, value, fld, category, mappedValue, weight)
    }
  })

  def makeItemset(reader: XMLEventReader, attrs: MetaData): Itemset = makeElem(reader, attrs, new ElemBuilder[Itemset] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): Itemset = {
      val id = attrs(AttrTags.ID)
      val support = attrs.getDouble(AttrTags.SUPPORT)
      val numberOfItems = attrs.getInt(AttrTags.NUMBER_OF_ITEMS)
      val itemRefs = makeElems(reader, ElemTags.ITEMSET, ElemTags.ITEM_REF, new ElemBuilder[ItemRef] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): ItemRef = {
          val itemRef = attrs(AttrTags.ITEM_REF)

          new ItemRef(itemRef)
        }
      })

      new Itemset(itemRefs.toSet, id, support, numberOfItems)
    }
  })

  def makeAssociationRule(reader: XMLEventReader, attrs: MetaData): AssociationRule = makeElem(reader, attrs,
    new ElemBuilder[AssociationRule] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): AssociationRule = {
        val antecedent = attrs(AttrTags.ANTECEDENT)
        val consequent = attrs(AttrTags.CONSEQUENT)
        val support = attrs.double(AttrTags.SUPPORT)
        val confidence = attrs.double(AttrTags.CONFIDENCE)
        val lift = attrs.getDouble(AttrTags.LIFT)
        val leverage = attrs.getDouble(AttrTags.LEVERAGE)
        val affinity = attrs.getDouble(AttrTags.AFFINITY)
        val id = attrs.get(AttrTags.ID)

        new AssociationRule(antecedent, consequent, support, confidence, lift, leverage, affinity, id)
      }
    })

  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): AssociationAttributes = {
    val attributes = super.makeAttributes(attrs)
    new AssociationAttributes(attributes,
      numberOfTransactions = attrs.int(AttrTags.NUMBER_OF_TRANSACTIONS),
      minimumSupport = attrs.double(AttrTags.MINIMUM_SUPPORT),
      minimumConfidence = attrs.double(AttrTags.MINIMUM_CONFIDENCE),
      numberOfItems = attrs.int(AttrTags.NUMBER_OF_ITEMS),
      numberOfItemsets = attrs.int(AttrTags.NUMBER_OF_ITEMSETS),
      numberOfRules = attrs.int(AttrTags.NUMBER_OF_RULES),
      maxNumberOfItemsPerTA = attrs.getInt(AttrTags.MAX_NUMBER_OF_ITEMS_PER_TA),
      avgNumberOfItemsPerTA = attrs.getDouble(AttrTags.AVG_NUMBER_OF_ITEMS_PER_TA),
      lengthLimit = attrs.getInt(AttrTags.LENGTH_LIMIT)
    )
  }

  /** Name of the builder. */
  override def name: String = ElemTags.ASSOCIATION_MODEL
}

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

import org.pmml4s.NotSupportedException
import org.pmml4s.common.{ModelAttributes, Predicate}
import org.pmml4s.metadata.Field
import org.pmml4s.model.MultipleModelMethod.MultipleModelMethod
import org.pmml4s.model._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Builder of Mining Model
 */
class MiningBuilder extends Builder[MiningModel] {
  protected var attributes: ModelAttributes = _
  private val embeddedModels = mutable.ArrayBuilder.make[EmbeddedModel]
  private var segmentation: Segmentation = _
  private var miningModel: MutableMiningModel = _

  /** Builds a Mining model from a specified XML reader, which points the element <MiningModel> */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): MiningModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)
    this.miningModel = new MutableMiningModel(parent)

    traverseModel(reader, ElemTags.MINING_MODEL, {
      case event: EvElemStart => {
        miningModel.localTransformations = localTransformations
        miningModel.miningSchema = miningSchema
        event match {
          case EvElemStart(_, ElemTags.REGRESSION, attrs, _)    => embeddedModels += ???
          case EvElemStart(_, ElemTags.DECISION_TREE, attrs, _) => embeddedModels += ???
          case EvElemStart(_, ElemTags.SEGMENTATION, attrs, _)  => {
            segmentation = makeSegmentation(reader, attrs)
          }
          case _                                                =>
        }
      }
    })

    new MiningModel(parent, attributes, miningSchema,
      embeddedModels.result(), Option(segmentation),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  override def postBuild(): Unit = {
    super.postBuild()
    if (wrappedFields.nonEmpty && segmentation != null) {
      var i = -1
      val idToModel = segmentation.segments.map(x => {
        i += 1
        (x.id.getOrElse(i.toString), x.model)
      }).toMap
      output.foreach(x => {
        x.outputFields.foreach(of => {
          if (of.segmentId.isDefined) {
            val model = idToModel.get(of.segmentId.get)
            model.foreach(y => {
              wrappedFields.foreach(z => if (z.field == null) {
                val f = y.output.flatMap(_.getField(z.name))
                if (f.isDefined)
                  z.field = f.get
              })
            })
          }
        })
      })
    }
  }

  private def makeSegmentation(reader: XMLEventReader, attrs: XmlAttrs): Segmentation = makeElem(reader, attrs, new ElemBuilder[Segmentation] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): Segmentation = {
      val multipleModelMethod = MultipleModelMethod.withName(attrs(AttrTags.MULTIPLE_MODEL_METHOD))
      miningModel.multipleModelMethod = multipleModelMethod
      val missingPredictionTreatment = attrs.get(AttrTags.MISSING_PREDICTION_TREATMENT).map(MissingPredictionTreatment.withName(_)).getOrElse(
        MissingPredictionTreatment.continue
      )
      val missingThreshold = attrs.getDouble(AttrTags.MISSING_THRESHOLD, 1)

      val segments = makeElems(reader, ElemTags.SEGMENTATION, ElemTags.SEGMENT, new ElemBuilder[Segment] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Segment = {
          val id = attrs.get(AttrTags.ID)
          val weight = attrs.getDouble(AttrTags.WEIGHT, 1.0)
          var predicate: Predicate = null
          var model: Model = null
          var variableWeight: VariableWeight = null

          traverseElems(reader, ElemTags.SEGMENT, {
            case event: EvElemStart if Predicate.contains(event.label)                                => predicate = makePredicate(reader, event)
            case EvElemStart(_, label, attrs, _) if ModelBuilder.PMML_SUPPORTED_MODELS contains label => {
              val builder = Builder.get(label).getOrElse(throw new NotSupportedException(label))
              model = builder.build(reader, attrs, miningModel)
              builder.postBuild()
            }
            case EvElemStart(_, ElemTags.VARIABLE_WEIGHT, attrs, _)                                   => variableWeight = makeElem(reader, attrs, new ElemBuilder[VariableWeight] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): VariableWeight = {
                val fld = getField(attrs(AttrTags.FIELD)) getOrElse model.output.map(_.field(attrs(AttrTags.FIELD))).get
                new VariableWeight(fld)
              }
            })
            case _                                                                                    =>
          })

          val segment = new Segment(predicate, model, Option(variableWeight), id, weight)
          miningModel += segment
          segment
        }
      })

      new Segmentation(multipleModelMethod, segments, missingPredictionTreatment, missingThreshold)
    }
  })

  override def getField(name: String): Option[Field] = {
    val result: Option[Field] = miningModel.getField(name)
    result orElse super.getField(name)
  }

  /** Name of the builder. */
  override def name: String = ElemTags.MINING_MODEL
}

class MutableMiningModel extends MutableModel {
  def this(parent: Model) = {
    this()
    this.parent = parent
  }

  var multipleModelMethod: MultipleModelMethod = _

  val segments = new ArrayBuffer[Segment]

  def +=(segment: Segment): this.type = {
    segments += segment
    this
  }

  /** Returns the field of a given name, None if a field with the given name does not exist. */
  override def getField(name: String): Option[Field] = {
    if (multipleModelMethod != null && multipleModelMethod == MultipleModelMethod.modelChain) {
      for (i <- (0 until segments.size).reverse) {
        val f = segments(i).model.output.flatMap(_.getField(name))
        if (f.isDefined)
          return f
      }
    }

    super.getField(name)
  }
}


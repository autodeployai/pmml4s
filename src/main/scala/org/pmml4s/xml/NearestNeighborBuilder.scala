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

import org.pmml4s.common.{CompareFunction, ComparisonMeasure, Table}
import org.pmml4s.model._

/**
 * Builder of Nearest Neighbor model.
 */
class NearestNeighborBuilder extends Builder[NearestNeighborModel] {
  protected var attributes: NearestNeighborAttributes = _
  private var trainingInstances: TrainingInstances = _
  private var comparisonMeasure: ComparisonMeasure = _
  private var knnInputs: KNNInputs = _

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): NearestNeighborModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, name, {
      case EvElemStart(_, ElemTags.TRAINING_INSTANCES, attrs, _) =>
        trainingInstances = makeTrainingInstances(reader, attrs)
      case EvElemStart(_, ElemTags.COMPARISON_MEASURE, attrs, _) =>
        comparisonMeasure = makeComparisonMeasure(reader, attrs)
      case EvElemStart(_, ElemTags.KNN_INPUTS, attrs, _)         =>
        knnInputs = makeKNNInputs(reader, attrs)
    })

    new NearestNeighborModel(parent, attributes, miningSchema,
      trainingInstances, comparisonMeasure, knnInputs,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeTrainingInstances(reader: XMLEventReader, attrs: XmlAttrs): TrainingInstances = makeElem(reader, attrs,
    new ElemBuilder[TrainingInstances] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): TrainingInstances = {
        val isTransformed = attrs.getBoolean(AttrTags.IS_TRANSFORMED, false)
        val recordCount = attrs.getInt(AttrTags.RECORD_COUNT)
        val fieldCount = attrs.getInt(AttrTags.FIELD_COUNT)
        var instanceFields: InstanceFields = null
        var table: Table = null
        traverseElems(reader, ElemTags.TRAINING_INSTANCES, {
          case EvElemStart(_, ElemTags.INSTANCE_FIELDS, attrs, _) => instanceFields =
            makeElem(reader, attrs, new ElemBuilder[InstanceFields] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): InstanceFields = {
                val instanceFields = makeElems(reader, ElemTags.INSTANCE_FIELDS, ElemTags.INSTANCE_FIELD,
                  new ElemBuilder[InstanceField] {
                    override def build(reader: XMLEventReader, attrs: XmlAttrs): InstanceField = {
                      val field = attrs(AttrTags.FIELD)
                      val column = attrs.get(AttrTags.COLUMN)

                      new InstanceField(field, column)
                    }
                  })

                new InstanceFields(instanceFields)
              }
            })
          case event: EvElemStart if Table.contains(event.label)  => table = makeTable(reader, event)
        })

        new TrainingInstances(instanceFields, table, isTransformed, recordCount, fieldCount)
      }
    })

  def makeKNNInputs(reader: XMLEventReader, attrs: XmlAttrs): KNNInputs = makeElem(reader, attrs,
    new ElemBuilder[KNNInputs] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): KNNInputs = {
        val knnInputs = makeElems(reader, ElemTags.KNN_INPUTS, ElemTags.KNN_INPUT, new ElemBuilder[KNNInput] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): KNNInput = {
            val f = field(attrs(AttrTags.FIELD))
            val fieldWeight = attrs.getDouble(AttrTags.FIELD_WEIGHT, 1.0)
            val compareFunction = attrs.get(AttrTags.COMPARE_FUNCTION).map(CompareFunction.withName(_))

            new KNNInput(f, compareFunction, fieldWeight)
          }
        })

        new KNNInputs(knnInputs)
      }
    })

  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): NearestNeighborAttributes = {
    val attributes = super.makeAttributes(attrs)

    new NearestNeighborAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      numberOfNeighbors = attrs.int(AttrTags.NUMBER_OF_NEIGHBORS),
      continuousScoringMethod = attrs.get(AttrTags.CONTINUOUS_SCORING_METHOD).map(x => ContScoringMethod.withName(x)).
        getOrElse(ContScoringMethod.average),
      categoricalScoringMethod = attrs.get(AttrTags.CATEGORICAL_SCORING_METHOD).map(x => CatScoringMethod.withName(x)).
        getOrElse(CatScoringMethod.majorityVote),
      instanceIdVariable = attrs.get(AttrTags.INSTANCE_ID_VARIABLE),
      threshold = attrs.getDouble(AttrTags.THRESHOLD, 0.001)
    )
  }

  /** Name of the builder. */
  override def name: String = ElemTags.NEAREST_NEIGHBOR_MODEL
}


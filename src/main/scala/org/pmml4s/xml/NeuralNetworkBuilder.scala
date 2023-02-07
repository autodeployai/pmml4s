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

import org.pmml4s.model._
import org.pmml4s.transformations.DerivedField

import scala.collection.mutable

/**
 * Builder of Neural Network model.
 */
class NeuralNetworkBuilder extends Builder[NeuralNetwork] {
  protected var attributes: NeuralNetworkAttributes = _
  private var neuralInputs: NeuralInputs = _
  private var neuralOutputs: NeuralOutputs = _
  private val neuralLayers = mutable.ArrayBuilder.make[NeuralLayer]

  /** Builds a Neural Network model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): NeuralNetwork = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.NEURAL_NETWORK, {
      case EvElemStart(_, ElemTags.NEURAL_INPUTS, attrs, _)  => neuralInputs = makeNeuralInputs(reader, attrs)
      case EvElemStart(_, ElemTags.NEURAL_LAYER, attrs, _)   => neuralLayers += makeNeuralLayer(reader, attrs)
      case EvElemStart(_, ElemTags.NEURAL_OUTPUTS, attrs, _) => neuralOutputs = makeNeuralOutputs(reader, attrs)
    })

    new NeuralNetwork(parent, attributes, miningSchema,
      neuralInputs, neuralLayers.result(), neuralOutputs,
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  private def makeNeuralInputs(reader: XMLEventReader, attrs: XmlAttrs): NeuralInputs = makeElem(reader, attrs, new ElemBuilder[NeuralInputs] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): NeuralInputs = {
      val numberOfInputs = attrs.getInt(AttrTags.NUMBER_OF_INPUTS)
      val neuralInputs = makeElems(reader, ElemTags.NEURAL_INPUTS, ElemTags.NEURAL_INPUT, new ElemBuilder[NeuralInput] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): NeuralInput = {
          val id = attrs(AttrTags.ID)
          val derivedField = makeElem(reader, ElemTags.NEURAL_INPUT, ElemTags.DERIVED_FIELD, new ElemBuilder[DerivedField] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): DerivedField = makeDerivedField(reader, attrs)
          })

          new NeuralInput(id, derivedField.get)
        }
      })

      new NeuralInputs(neuralInputs, numberOfInputs)
    }
  })

  private def makeNeuralLayer(reader: XMLEventReader, attrs: XmlAttrs): NeuralLayer = makeElem(reader, attrs, new ElemBuilder[NeuralLayer] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): NeuralLayer = {
      val numberOfNeurons = attrs.getInt(AttrTags.NUMBER_OF_NEURONS)
      val activationFunction = attrs.get(AttrTags.ACTIVATION_FUNCTION).map(ActivationFunction.withName(_))
      val threshold = attrs.getDouble(AttrTags.THRESHOLD)
      val width = attrs.getDouble(AttrTags.WIDTH)
      val altitude = attrs.getDouble(AttrTags.ALTITUDE)
      val normalizationMethod = attrs.get(AttrTags.NORMALIZATION_METHOD).map(NNNormalizationMethod.withName(_))
      val neurons = makeElems(reader, ElemTags.NEURAL_LAYER, ElemTags.NEURON, new ElemBuilder[Neuron] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): Neuron = {
          val id = attrs(AttrTags.ID)
          val bias = attrs.getDouble(AttrTags.BIAS)
          val width = attrs.getDouble(AttrTags.WIDTH)
          val altitude = attrs.getDouble(AttrTags.ALTITUDE)
          val cons = makeElems(reader, ElemTags.NEURON, ElemTags.CON, new ElemBuilder[Con] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): Con = {
              val from = attrs(AttrTags.FROM)
              val weight = attrs.double(AttrTags.WEIGHT)
              new Con(from, weight)
            }
          })

          new Neuron(cons, id, bias, width, altitude)
        }
      })

      new NeuralLayer(neurons, numberOfNeurons, activationFunction, threshold, width, altitude, normalizationMethod)
    }
  })

  private def makeNeuralOutputs(reader: XMLEventReader, attrs: XmlAttrs): NeuralOutputs = makeElem(reader, attrs, new ElemBuilder[NeuralOutputs] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): NeuralOutputs = {
      val numberOfOutputs = attrs.getInt(AttrTags.NUMBER_OF_OUTPUTS)
      val neuralOutputs = makeElems(reader, ElemTags.NEURAL_OUTPUTS, ElemTags.NEURAL_OUTPUT, new ElemBuilder[NeuralOutput] {
        override def build(reader: XMLEventReader, attrs: XmlAttrs): NeuralOutput = {
          val outputNeuron = attrs(AttrTags.OUTPUT_NEURON)
          val derivedField = makeElem(reader, ElemTags.NEURAL_OUTPUT, ElemTags.DERIVED_FIELD, new ElemBuilder[DerivedField] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): DerivedField = makeDerivedField(reader, attrs)
          })

          new NeuralOutput(outputNeuron, derivedField.get)
        }
      })

      new NeuralOutputs(neuralOutputs, numberOfOutputs)
    }
  })

  override def makeAttributes(attrs: XmlAttrs): NeuralNetworkAttributes = {
    val attributes = super.makeAttributes(attrs)

    new NeuralNetworkAttributes(
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable,
      activationFunction = ActivationFunction.withName(attrs(AttrTags.ACTIVATION_FUNCTION)),
      normalizationMethod = attrs.get(AttrTags.NORMALIZATION_METHOD).map(x => NNNormalizationMethod.withName(x)).getOrElse(NNNormalizationMethod.none),
      threshold = attrs.getDouble(AttrTags.THRESHOLD, 0.0),
      width = attrs.getDouble(AttrTags.WIDTH),
      altitude = attrs.getDouble(AttrTags.ALTITUDE, 1.0),
      numberOfLayers = attrs.getInt(AttrTags.NUMBER_OF_LAYERS)
    )
  }

  /** Name of the builder. */
  override def name: String = ElemTags.NEURAL_NETWORK
}


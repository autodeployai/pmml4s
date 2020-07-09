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
package org.pmml4s.model

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.common._
import org.pmml4s.data.Series
import org.pmml4s.metadata.{MiningSchema, Output, Targets}
import org.pmml4s.model.ActivationFunction.ActivationFunction
import org.pmml4s.model.NNNormalizationMethod.NNNormalizationMethod
import org.pmml4s.transformations._
import org.pmml4s.util.Utils

import scala.collection.{immutable, mutable}

/**
 * A neural network has one or more input nodes and one or more neurons. Some neurons' outputs are the output of the
 * network. The network is defined by the neurons and their connections, aka weights. All neurons are organized into
 * layers; the sequence of layers defines the order in which the activations are computed. All output activations for
 * neurons in some layer L are evaluated before computation proceeds to the next layer L+1. Note that this allows for
 * recurrent networks where outputs of neurons in layer L+i can be used as input in layer L where L+i > L. The model
 * does not define a specific evaluation order for neurons within a layer.
 */
class NeuralNetwork(
                     override var parent: Model,
                     override val attributes: NeuralNetworkAttributes,
                     override val miningSchema: MiningSchema,
                     val neuralInputs: NeuralInputs,
                     val neuralLayers: Array[NeuralLayer],
                     val neuralOutputs: NeuralOutputs,
                     override val output: Option[Output] = None,
                     override val targets: Option[Targets] = None,
                     override val localTransformations: Option[LocalTransformations] = None,
                     override val modelStats: Option[ModelStats] = None,
                     override val modelExplanation: Option[ModelExplanation] = None,
                     override val modelVerification: Option[ModelVerification] = None,
                     override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedNeuralNetworkAttributes {

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.NeuralNetwork

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val (series, returnInvalid) = prepare(values)
    if (returnInvalid) {
      return nullSeries
    }

    val transformedVals = new mutable.HashMap[String, Double]

    var i = 0
    while (i < neuralInputs.neuralInputs.length) {
      val neuralInput = neuralInputs.neuralInputs(i)
      val v = neuralInput.derivedField.eval(series)

      // check if it's missing
      if (Utils.isMissing(v)) {
        return nullSeries
      }
      transformedVals.put(neuralInput.id, Utils.toDouble(v))
      i += 1
    }

    for (layer <- neuralLayers) {
      val af = layer.activationFunction.getOrElse(activationFunction)
      for (neuron <- layer.neurons) {
        var z = 0.0
        if (af == ActivationFunction.radialBasis) {
          neuron.cons.foreach(x => {
            val a = transformedVals(x.from) - x.weight
            z += a * a
          })
          val w = neuron.width.getOrElse(layer.width.getOrElse(width.get))
          z /= (2.0 * w * w)

          val alt = neuron.altitude.getOrElse(layer.altitude.getOrElse(altitude))
          val f = neuron.cons.length
          val activation = Math.exp(f * Math.log(alt) - z)
          transformedVals.put(neuron.id, activation)
        }
        else {
          z = neuron.bias.getOrElse(0.0)
          for (con <- neuron.cons) {
            z += transformedVals(con.from) * con.weight
          }

          val activation = af match {
            case ActivationFunction.threshold   => if (z > layer.threshold.getOrElse(threshold)) 1.0 else 0.0
            case ActivationFunction.logistic    => 1.0 / (1.0 + Math.exp(-z))
            case ActivationFunction.tanh        => Math.tanh(z)
            case ActivationFunction.identity    => z
            case ActivationFunction.exponential => Math.exp(z)
            case ActivationFunction.reciprocal  => 1.0 / z
            case ActivationFunction.square      => z * z
            case ActivationFunction.Gauss       => Math.exp(-(z * z))
            case ActivationFunction.sine        => Math.sin(z)
            case ActivationFunction.cosine      => Math.cos(z)
            case ActivationFunction.Elliott     => z / (1.0 + Math.abs(z))
            case ActivationFunction.arctan      => 2.0 * Math.atan(z) / Math.PI
            case ActivationFunction.rectifier   => Math.max(0.0, z)
          }
          transformedVals.put(neuron.id, activation)
        }
      }

      import NNNormalizationMethod._
      val nm = layer.normalizationMethod.getOrElse(normalizationMethod)
      nm match {
        case `simplemax` => {
          val sum = layer.neurons.map(x => transformedVals(x.id)).sum
          layer.neurons.foreach(x => transformedVals.put(x.id, transformedVals(x.id) / sum))
        }
        case `softmax`   => {
          val sum = layer.neurons.map(x => Math.exp(transformedVals(x.id))).sum
          layer.neurons.foreach(x => transformedVals.put(x.id, Math.exp(transformedVals(x.id)) / sum))
        }
        case _           =>
      }
    }

    // NeuralNetwork allows multiple targets in a single PMML.
    val outputs = new GenericMultiModelOutputs
    for (neuralOutput <- neuralOutputs.neuralOutputs) {
      val normalizedVal = transformedVals(neuralOutput.outputNeuron)
      val f = neuralOutput.derivedField.getDataField
      if (f.isDefined) {
        val t = f.get
        if (t.isContinuous) {
          val regOutputs = outputs.getOrInsert[NeuralNetworkOutputs](t.name, createOutputs)
          regOutputs.setPredictedValue(neuralOutput.derivedField.deeval(normalizedVal))
        } else {
          val cls = getClass(neuralOutput.derivedField.expr)
          if (cls.isDefined) {
            val clsOutputs = outputs.getOrInsert[NeuralNetworkOutputs](t.name, createOutputs)
            clsOutputs.putProbability(cls.get, normalizedVal)
          }
        }
      }
    }

    outputs.toSeq.foreach(x => x._2 match {
      case clsOutputs: NeuralNetworkOutputs => clsOutputs.evalPredictedValueByProbabilities(classes(x._1))
      case _                                =>
    })

    if (singleTarget) {
      result(series, outputs.toSeq.head._2)
    } else {
      result(series, outputs)
    }
  }

  /** Creates an object of NeuralNetworkOutputs that is for writing into an output series.  */
  override def createOutputs(): NeuralNetworkOutputs = new NeuralNetworkOutputs

  private def getClass(expr: Expression): Option[Any] = expr match {
    case nd: NormDiscrete => Some(nd.value)
    case fr: FieldRef     => if (fr.field.isDerivedField) {
      getClass(fr.field.asInstanceOf[DerivedField].expr)
    } else None
    case _                => None
  }
}

object ActivationFunction extends Enumeration {
  type ActivationFunction = Value
  val threshold, logistic, tanh, identity, exponential, reciprocal, square, Gauss, sine, cosine, Elliott, arctan, rectifier, radialBasis = Value
}

/**
 * A normalization method softmax ( pj = exp(yj) / Sumi(exp(yi) ) ) or simplemax ( pj = yj / Sumi(yi) ) can be applied
 * to the computed activation values. The attribute normalizationMethod is defined for the network with default value
 * none ( pj = yj ), but can be specified for each layer as well. Softmax normalization is most often applied to the
 * output layer of a classification network to get the probabilities of all answers. Simplemax normalization is often
 * applied to the hidden layer consisting of elements with radial basis activation function to get a "normalized RBF"
 * activation.
 */
object NNNormalizationMethod extends Enumeration {
  type NNNormalizationMethod = Value
  val none, simplemax, softmax = Value
}


trait HasNeuralNetworkAttributes extends HasModelAttributes {

  def activationFunction: ActivationFunction

  def normalizationMethod: NNNormalizationMethod

  def threshold: Double

  def width: Option[Double]

  def altitude: Double

  def numberOfLayers: Option[Int]
}

trait HasWrappedNeuralNetworkAttributes extends HasWrappedModelAttributes with HasNeuralNetworkAttributes {

  override def attributes: NeuralNetworkAttributes

  def activationFunction: ActivationFunction = attributes.activationFunction

  def normalizationMethod: NNNormalizationMethod = attributes.normalizationMethod

  def threshold: Double = attributes.threshold

  def width: Option[Double] = attributes.width

  def altitude: Double = attributes.altitude

  def numberOfLayers: Option[Int] = attributes.numberOfLayers
}

class NeuralNetworkAttributes(
                               override val functionName: MiningFunction,
                               val activationFunction: ActivationFunction,
                               val normalizationMethod: NNNormalizationMethod = NNNormalizationMethod.none,
                               val threshold: Double = 0.0,
                               val width: Option[Double] = None,
                               val altitude: Double = 1.0,
                               val numberOfLayers: Option[Int] = None,
                               override val modelName: Option[String] = None,
                               override val algorithmName: Option[String] = None,
                               override val isScorable: Boolean = true
                             ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable) with HasNeuralNetworkAttributes

/**
 * An input neuron represents the normalized value for an input field. A numeric input field is usually mapped to a
 * single input neuron while a categorical input field is usually mapped to a set of input neurons using some fan-out
 * function. The normalization is defined using the elements NormContinuous and NormDiscrete defined in the
 * Transformation Dictionary. The element DerivedField is the general container for these transformations.
 */
class NeuralInputs(val neuralInputs: Array[NeuralInput], val numberOfInputs: Option[Int]) extends PmmlElement

/**
 * Defines how input fields are normalized so that the values can be processed in the neural network. For example,
 * string values must be encoded as numeric values.
 */
class NeuralInput(val id: String, val derivedField: DerivedField) extends PmmlElement

class NeuralLayer(
                   val neurons: Array[Neuron],
                   val numberOfNeurons: Option[Int] = None,
                   val activationFunction: Option[ActivationFunction] = None,
                   val threshold: Option[Double] = None,
                   val width: Option[Double] = None,
                   val altitude: Option[Double] = None,
                   val normalizationMethod: Option[NNNormalizationMethod] = None
                 ) extends PmmlElement

class NeuralOutputs(val neuralOutputs: Array[NeuralOutput], val numberOfOutputs: Option[Int]) extends PmmlElement

/**
 * Defines how the output of the neural network must be interpreted.
 */
class NeuralOutput(val outputNeuron: String, val derivedField: DerivedField) extends PmmlElement

/**
 * Contains an identifier id which must be unique in all layers. The attribute bias implicitly defines a connection to
 * a bias unit where the unit's value is 1.0 and the weight is the value of bias. The activation function and
 * normalization method for Neuron can be defined in NeuralLayer. If either one is not defined for the layer then the
 * default one specified for NeuralNetwork applies. If the activation function is radialBasis, the attribute width must
 * be specified either in Neuron, NeuralLayer or NeuralNetwork. Again, width specified in Neuron will override a
 * respective value from NeuralLayer, and in turn will override a value given in NeuralNetwork.
 *
 * Weighted connections between neural net nodes are represented by Con elements.
 */
class Neuron(
              val cons: Array[Con],
              val id: String,
              val bias: Option[Double] = None,
              val width: Option[Double] = None,
              val altitude: Option[Double] = None) extends PmmlElement

/** Defines the connections coming into that parent element. The neuron identified by from may be part of any layer. */
class Con(val from: String, val weight: Double) extends PmmlElement

class NeuralNetworkOutputs extends MixedClsWithRegOutputs

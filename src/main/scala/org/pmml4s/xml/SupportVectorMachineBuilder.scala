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

import org.pmml4s.SemanticErrorException
import org.pmml4s.common.{DenseVector, DoubleEvaluator, Vector}
import org.pmml4s.model._

import scala.collection.mutable
import scala.xml.MetaData
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Builder of Support Vector Machine model
 */
class SupportVectorMachineBuilder extends Builder[SupportVectorMachineModel] {
  protected var attributes: SupportVectorMachineAttributes = _
  private var kernelType: KernelType = _
  private var vectorDictionary: VectorDictionary = _
  private val supportVectorMachines = mutable.ArrayBuilder.make[SupportVectorMachine]()

  /** Builds a PMML model from a specified XML reader. */
  override def build(reader: XMLEventReader, attrs: XmlAttrs, parent: Model): SupportVectorMachineModel = {
    this.parent = parent
    this.attributes = makeAttributes(attrs)

    traverseModel(reader, ElemTags.SUPPORT_VECTOR_MACHINE_MODEL, {
      case event: EvElemStart if (KernelType.contains(event.label))  => kernelType = makeKernelType(reader, event)
      case EvElemStart(_, ElemTags.VECTOR_DICTIONARY, attrs, _)      => vectorDictionary = makeVectorDictionary(reader, attrs)
      case EvElemStart(_, ElemTags.SUPPORT_VECTOR_MACHINE, attrs, _) => supportVectorMachines += makeSupportVectorMachine(reader, attrs)
    })

    new SupportVectorMachineModel(parent, attributes, miningSchema,
      kernelType, vectorDictionary, supportVectorMachines.result(),
      output, targets, localTransformations, modelStats, modelExplanation, modelVerification, extensions.toIndexedSeq)
  }

  def makeSupportVectorMachine(reader: XMLEventReader, attrs: MetaData): SupportVectorMachine = makeElem(reader, attrs, new ElemBuilder[SupportVectorMachine] {
    override def build(reader: XMLEventReader, attrs: XmlAttrs): SupportVectorMachine = {
      val targetCategory = attrs.get(AttrTags.TARGET_CATEGORY).map(target.toVal(_))
      val alternateTargetCategory = attrs.get(AttrTags.ALTERNATE_TARGET_CATEGORY).map(target.toVal(_))
      val threshold = attrs.getDouble(AttrTags.THRESHOLD)
      var supportVectors: SupportVectors = null
      var coefficients: Coefficients = null
      traverseElems(reader, ElemTags.SUPPORT_VECTOR_MACHINE, {
        case EvElemStart(_, ElemTags.SUPPORT_VECTORS, attrs, _) => supportVectors = makeElem(reader, attrs,
          new ElemBuilder[SupportVectors] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): SupportVectors = {
              val numberOfSupportVectors = attrs.getInt(AttrTags.NUMBER_OF_SUPPORT_VECTORS)
              val numberOfAttributes = attrs.getInt(AttrTags.NUMBER_OF_ATTRIBUTES)
              val supportVectors = makeElems(reader, ElemTags.SUPPORT_VECTORS, ElemTags.SUPPORT_VECTOR, new ElemBuilder[SupportVector] {
                override def build(reader: XMLEventReader, attrs: XmlAttrs): SupportVector = {
                  val vectorId = attrs(AttrTags.VECTOR_ID)
                  new SupportVector(vectorId)
                }
              }, numberOfSupportVectors)

              numberOfSupportVectors.foreach(x => if (x != supportVectors.length) throw
                new SemanticErrorException(s"The number of SupportVector elements must be $x, got ${supportVectors.length}"))
              numberOfAttributes.foreach(x => if (supportVectors.map(y => vectorDictionary(y.vectorId)).exists(z => z.numAttrs != x)) throw
                new SemanticErrorException(s"The number of attributes in the support vectors (which all must have the same length) must be $x"))
              new SupportVectors(supportVectors)
            }
          })
        case EvElemStart(_, ElemTags.COEFFICIENTS, attrs, _)    => coefficients = makeElem(reader, attrs, new ElemBuilder[Coefficients] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): Coefficients = {
            val numberOfCoefficients = attrs.getInt(AttrTags.NUMBER_OF_COEFFICIENTS)
            val absoluteValue = attrs.getDouble(AttrTags.ABSOLUTE_VALUE, .0)
            val coefficients = makeElems(reader, ElemTags.COEFFICIENTS, ElemTags.COEFFICIENT, new ElemBuilder[Coefficient] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): Coefficient = {
                val value = attrs.getDouble(AttrTags.VALUE, .0)
                new Coefficient(value)
              }
            }, numberOfCoefficients)

            numberOfCoefficients.foreach(x => if (x != coefficients.length) throw
              new SemanticErrorException(s"The number of oefficients must be $x, got ${coefficients.length}"))
            new Coefficients(coefficients, absoluteValue)
          }
        })
      })

      new SupportVectorMachine(Option(supportVectors), coefficients, targetCategory, alternateTargetCategory, threshold)
    }
  })

  def makeVectorDictionary(reader: XMLEventReader, attrs: MetaData): VectorDictionary =
    makeElem(reader, attrs, new ElemBuilder[VectorDictionary] {
      override def build(reader: XMLEventReader, attrs: XmlAttrs): VectorDictionary = {
        val numberOfVectors = attrs.getInt(AttrTags.NUMBER_OF_VECTORS)
        var vectorFields: VectorFields = null
        val vectorInstances = mutable.ArrayBuilder.make[VectorInstance]()
        numberOfVectors.foreach(vectorInstances.sizeHint(_))
        traverseElems(reader, ElemTags.VECTOR_DICTIONARY, {
          case EvElemStart(_, ElemTags.VECTOR_FIELDS, attrs, _)   => vectorFields = makeElem(reader, attrs,
            new ElemBuilder[VectorFields] {
              override def build(reader: XMLEventReader, attrs: XmlAttrs): VectorFields = {
                val numberOfFields = attrs.getInt(AttrTags.NUMBER_OF_FIELDS)
                val fields = mutable.ArrayBuilder.make[DoubleEvaluator]
                numberOfFields.foreach(fields.sizeHint(_))
                traverseElems(reader, ElemTags.VECTOR_FIELDS, {
                  case EvElemStart(_, ElemTags.FIELD_REF, attrs, _)             => fields += makeFieldRef(reader, attrs, scope)
                  case EvElemStart(_, ElemTags.CATEGORICAL_PREDICTOR, attrs, _) => fields += makeCategoricalPredictor(reader, attrs)
                })

                val fs = fields.result()
                numberOfFields.foreach(x => if (x != fs.length) throw
                  new SemanticErrorException(s"The number of entries in VectorFields must be $x, got ${fs.length}"))
                new VectorFields(fs)
              }
            })
          case EvElemStart(_, ElemTags.VECTOR_INSTANCE, attrs, _) => vectorInstances += makeElem(reader, attrs, new ElemBuilder[VectorInstance] {
            override def build(reader: XMLEventReader, attrs: XmlAttrs): VectorInstance = {
              val id = attrs(AttrTags.ID)
              var array: Vector[Double] = null
              traverseElems(reader, ElemTags.VECTOR_INSTANCE, {
                case EvElemStart(_, ElemTags.ARRAY, attrs, _)             => array = makeElem(reader, attrs,
                  new ElemBuilder[Vector[Double]] {
                    override def build(reader: XMLEventReader, attrs: XmlAttrs): Vector[Double] = {
                      val values = makeRealArray(reader, attrs)
                      new DenseVector[Double](values)
                    }
                  })
                case EvElemStart(_, ElemTags.REAL_SPARSE_ARRAY, attrs, _) => array = makeElem(reader, attrs,
                  new ElemBuilder[Vector[Double]] {
                    override def build(reader: XMLEventReader, attrs: XmlAttrs): Vector[Double] =
                      makeRealSparseArray(reader, attrs)
                  })
              })
              new VectorInstance(id, array)
            }
          })
        })

        val vs = vectorInstances.result()
        numberOfVectors.foreach(x => if (x != vs.length) throw
          new SemanticErrorException(s"The number of vectors must be $x, got ${vs.length}"))
        new VectorDictionary(vectorFields, vs)
      }
    })

  def makeKernelType(reader: XMLEventReader, event: EvElemStart): KernelType = makeElem(reader, event,
    new GroupElemBuilder[KernelType] {
      override def build(reader: XMLEventReader, event: EvElemStart): KernelType = event match {
        case EvElemStart(_, ElemTags.LINEAR_KERNEL_TYPE, attrs, _)       => makeElem(reader, attrs, new ElemBuilder[LinearKernelType] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): LinearKernelType = {
            val description = attrs.get(AttrTags.DESCRIPTION)
            new LinearKernelType(description)
          }
        })
        case EvElemStart(_, ElemTags.POLYNOMIAL_KERNEL_TYPE, attrs, _)   => makeElem(reader, attrs, new ElemBuilder[PolynomialKernelType] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): PolynomialKernelType = {
            val description = attrs.get(AttrTags.DESCRIPTION)
            val gamma = attrs.getDouble(AttrTags.GAMMA, 1.0)
            val coef0 = attrs.getDouble(AttrTags.COEF0, 1.0)
            val degree = attrs.getDouble(AttrTags.DEGREE, 1.0)
            new PolynomialKernelType(gamma, coef0, degree, description)
          }
        })
        case EvElemStart(_, ElemTags.RADIAL_BASIS_KERNEL_TYPE, attrs, _) => makeElem(reader, attrs, new ElemBuilder[RadialBasisKernelType] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): RadialBasisKernelType = {
            val description = attrs.get(AttrTags.DESCRIPTION)
            val gamma = attrs.getDouble(AttrTags.GAMMA, 1.0)
            new RadialBasisKernelType(gamma, description)
          }
        })
        case EvElemStart(_, ElemTags.SIGMOID_KERNEL_TYPE, attrs, _)      => makeElem(reader, attrs, new ElemBuilder[SigmoidKernelType] {
          override def build(reader: XMLEventReader, attrs: XmlAttrs): SigmoidKernelType = {
            val description = attrs.get(AttrTags.DESCRIPTION)
            val gamma = attrs.getDouble(AttrTags.GAMMA, 1.0)
            val coef0 = attrs.getDouble(AttrTags.COEF0, 1.0)
            new SigmoidKernelType(gamma, coef0, description)
          }
        })
        case _                                                           => ??????
      }
    })

  /** Extracts these common attributes from a model */
  override protected def makeAttributes(attrs: XmlAttrs): SupportVectorMachineAttributes = {
    val attributes = super.makeAttributes(attrs)

    new SupportVectorMachineAttributes(
      threshold = attrs.getDouble(AttrTags.THRESHOLD, 0.0),
      svmRepresentation = attrs.get(AttrTags.SVM_REPRESENTATION).map(SVMRepresentation.withName(_)).getOrElse(SVMRepresentation.SupportVectors),
      classificationMethod = attrs.get((AttrTags.CLASSIFICATION_METHOD)).map(SVMClassificationMethod.withName(_)).getOrElse(SVMClassificationMethod.OneAgainstAll),
      maxWins = attrs.getBoolean(AttrTags.MAX_WINS, false),
      functionName = attributes.functionName,
      modelName = attributes.modelName,
      algorithmName = attributes.algorithmName,
      isScorable = attributes.isScorable)
  }

  /** Name of the builder. */
  override def name: String = ElemTags.SUPPORT_VECTOR_MACHINE_MODEL
}

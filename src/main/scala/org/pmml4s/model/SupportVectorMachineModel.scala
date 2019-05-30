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
import org.pmml4s.model.SVMClassificationMethod.SVMClassificationMethod
import org.pmml4s.model.SVMRepresentation.SVMRepresentation
import org.pmml4s.transformations.LocalTransformations
import org.pmml4s.util.{MathUtils, Utils}
import org.pmml4s.xml.ElemTags

import scala.collection.immutable

/**
 * Support Vector Machine models for classification and regression are considered. A Support Vector Machine is a
 * function f which is defined in the space spanned by the kernel basis functions K(x,xi) of the support vectors xi:
 * f(x) = Sum_(i=1)n αi*K(x,xi) + b.
 *
 * Here n is the number of all support vectors, αi are the basis coefficients and b is the absolute coefficient. In an
 * equivalent interpretation, n could also be considered as the total number of all training vectors xi. Then the
 * support vectors are the subset of all those vectors xi whose coefficients αi are greater than zero. The term Support
 * Vector (SV) has also a geometrical interpretation because these vectors really support the discrimination function
 * f(x) = 0 in the mechanical interpretation.
 */
class SupportVectorMachineModel(
                                 override var parent: Model,
                                 override val attributes: SupportVectorMachineAttributes,
                                 override val miningSchema: MiningSchema,
                                 val kernelType: KernelType,
                                 val vectorDictionary: VectorDictionary,
                                 val supportVectorMachines: Array[SupportVectorMachine],
                                 override val output: Option[Output] = None,
                                 override val targets: Option[Targets] = None,
                                 override val localTransformations: Option[LocalTransformations] = None,
                                 override val modelStats: Option[ModelStats] = None,
                                 override val modelExplanation: Option[ModelExplanation] = None,
                                 override val modelVerification: Option[ModelVerification] = None,
                                 override val extensions: immutable.Seq[Extension] = immutable.Seq.empty)
  extends Model with HasWrappedSupportVectorMachineAttributes {

  require((isRegression || (isClassification && supportVectorMachines.forall(_.targetCategory.isDefined))),
    "The attribute targetCategory is required for classification models and gives the corresponding class label")

  require((isRegression || (isClassification && (classificationMethod == SVMClassificationMethod.OneAgainstOne ||
    (classificationMethod == SVMClassificationMethod.OneAgainstAll && isBinary && supportVectorMachines.length == 1)) &&
    supportVectorMachines.forall(_.alternateTargetCategory.isDefined))),
    "The attribute alternateTargetCategory is required in case of binary classification models with only one " +
      "SupportVectorMachine element. It is also required in case of multi-class classification models implementing the " +
      "one-against-one method")

  supportVectorMachines.foreach(_.init(vectorDictionary, svmRepresentation))

  /** Model element type. */
  override def modelElement: ModelElement = ModelElement.SupportVectorMachineModel

  /** Predicts values for a given data series. */
  override def predict(values: Series): Series = {
    val series = prepare(values)

    vectorDictionary.vectorFields.eval(series) match {
      case Right(xs) => {
        val outputs = createOutputs()
        outputs.predictedValue = if (isClassification) {
          if (classificationMethod == SVMClassificationMethod.OneAgainstOne ||
            (classificationMethod == SVMClassificationMethod.OneAgainstAll && isBinary && supportVectorMachines.length == 1)) {
            val values = supportVectorMachines.map(_.predict(xs, kernelType, maxWins, threshold))
            val votes = Utils.reduceByKey(values.map(x => (x, 1)))
            // compute probabilities
            outputs.probabilities = classes.map(x => (x, votes.getOrElse(x, 0) / values.length.toDouble)).toMap

            votes.maxBy(_._2)._1
          } else {
            val values = supportVectorMachines.map(_.eval(xs, kernelType)).zip(supportVectorMachines.map(_.targetCategory.get))
            if (maxWins) values.maxBy(_._1)._2 else values.minBy(_._1)._2
          }
        } else {
          supportVectorMachines.head.eval(xs, kernelType)
        }

        result(series, outputs)
      }
      case Left(_)   => nullSeries
    }
  }

  /** Tests if probabilities of categories of target can be produced by this model. */
  override def probabilitiesSupported: Boolean = classificationMethod == SVMClassificationMethod.OneAgainstOne

  /** Creates an object of SVMOutputs that is for writing into an output series.  */
  override def createOutputs(): SVMOutputs = new SVMOutputs
}

trait KernelType {
  def description: Option[String]

  def compute(x: Array[Double], y: Vector[Double]): Double
}

object KernelType {

  import ElemTags._

  val values = Set(LINEAR_KERNEL_TYPE, POLYNOMIAL_KERNEL_TYPE, RADIAL_BASIS_KERNEL_TYPE, SIGMOID_KERNEL_TYPE)

  def contains(s: String) = values.contains(s)
}

/**
 * Linear basis functions which lead to a hyperplane as classifier.
 * K(x,y) = <x,y>
 */
class LinearKernelType(val description: Option[String] = None) extends KernelType with PmmlElement {
  override def compute(x: Array[Double], y: Vector[Double]): Double = {
    MathUtils.product(x, y)
  }
}

/**
 * Polynomial basis functions which lead to a polynome classifier.
 * K(x,y) = (gamma*<x,y>+coef0)degree
 */
class PolynomialKernelType(val gamma: Double = 1.0,
                           val coef0: Double = 1.0,
                           val degree: Double = 1.0,
                           val description: Option[String] = None) extends KernelType with PmmlElement {
  override def compute(x: Array[Double], y: Vector[Double]): Double = {
    Math.pow(gamma * MathUtils.product(x, y) + coef0, degree)
  }
}

/**
 * Radial basis functions, the most common kernel type
 * K(x,y) = exp(-gamma*||x - y||2)
 */
class RadialBasisKernelType(val gamma: Double = 1.0, val description: Option[String] = None) extends KernelType with PmmlElement {
  override def compute(x: Array[Double], y: Vector[Double]): Double = {
    var res = 0.0
    for (i <- 0 until x.length) {
      res += (x(i) - y(i)) * (x(i) - y(i))
    }
    Math.exp(-gamma * res)
  }
}

/**
 * Sigmoid kernel functions for some models of Neural Network type
 * K(x,y) = tanh(gamma*<x,y>+coef0)
 */
class SigmoidKernelType(val gamma: Double = 1.0,
                        val coef0: Double = 1.0,
                        val description: Option[String] = None) extends KernelType with PmmlElement {
  override def compute(x: Array[Double], y: Vector[Double]): Double = {
    Math.tanh(gamma * MathUtils.product(x, y) + coef0)
  }
}

/**
 * Contains the set of support vectors which are of the typeVectorInstance.
 */
class VectorDictionary(val vectorFields: VectorFields,
                       val vectorInstances: Array[VectorInstance]) extends PmmlElement {
  private val map = vectorInstances.map(x => (x.id, x)).toMap

  def apply(id: String): VectorInstance = map(id)
}

/**
 * Defines which entries in the vectors correspond to which fields.
 */
class VectorFields(val vectorFields: Array[DoubleEvaluator]) extends PmmlElement {
  def eval(series: Series): Either[Boolean, Array[Double]] = {
    val res = new Array[Double](vectorFields.length)
    for (i <- 0 until vectorFields.length) {
      res(i) = vectorFields(i).asDouble(series)
      if (Utils.isMissing(res(i))) {
        return Left(true)
      }
    }
    Right(res)
  }
}

/**
 * A data vector given in dense or sparse array format. The order of the values corresponds to that of the VectorFields.
 * The sizes of the sparse arrays must match the number of fields included in the VectorFields element.
 */
class VectorInstance(val id: String, val array: Vector[Double]) extends PmmlElement {
  def numAttrs: Int = array.length
}

/**
 * Holds a single instance of an SVM.
 *
 * SupportVectors holds the support vectors as references towards VectorDictionary used by the respective SVM instance.
 * For storing the SVM coefficients, the element Coefficients is used. Both are combined in the element
 * SupportVectorMachine, which holds a single instance of an SVM.
 *
 * The attribute targetCategory is required for classification models and gives the corresponding class label. This
 * attribute is to be used for classification models implementing the one-against-all method. In this method, for n
 * classes, there are exactly n SupportVectorMachine elements. Depending on the model attribute maxWins, the SVM with
 * the largest or the smallest value determines the predicted class label.
 *
 * The attribute alternateTargetCategory is required in case of binary classification models with only one
 * SupportVectorMachine element. It is also required in case of multi-class classification models implementing the
 * one-against-one method. In this method, for n classes, there are exactly n(n-1)/2 SupportVectorMachine elements where
 * each SVM is trained on data from two classes. The first class is represented by the targetCategory attribute and the
 * second class by the alternateTargetCategory attribute. The predicted class label is determined based on a voting
 * scheme in which the category with the maximum number of votes wins. In case of a tie, the predicted class label is
 * the first category with maximal number of votes. For both cases (binary classification and multi-class classification
 * with one-against-one), the corresponding class labels are determined by comparing the numeric prediction with the
 * threshold. If maxWins is true and the prediction is larger than the threshold or maxWins is false and the prediction
 * is smaller than the threshold, the class label is the targetCategory attribute, otherwise, it is the
 * alternateTargetCategory attribute.
 *
 * Note that each SupportVectorMachine element may have its own threshold that overrides the default.
 */
class SupportVectorMachine(val supportVectors: Option[SupportVectors],
                           val coefficients: Coefficients,
                           val targetCategory: Option[Any],
                           val alternateTargetCategory: Option[Any],
                           val threshold: Option[Double]) extends PmmlElement {
  private val coeffs: Array[Double] = coefficients.coefficients.map(_.value)
  private var vectors: Array[Vector[Double]] = _

  def init(vectorDictionary: VectorDictionary, svmRepresentation: SVMRepresentation): Unit = {
    if (svmRepresentation == SVMRepresentation.SupportVectors) {
      vectors = supportVectors.get.supportVectors.map(x => vectorDictionary(x.vectorId).array)
    }
  }

  def eval(xs: Array[Double], kernelType: KernelType): Double = {
    if (vectors != null) {
      MathUtils.product(vectors.map(x => kernelType.compute(xs, x)), coeffs) + coefficients.absoluteValue
    } else {
      MathUtils.product(xs, coeffs) + coefficients.absoluteValue
    }
  }

  def predict(xs: Array[Double], kernelType: KernelType, maxWins: Boolean, threshold: Double): Any = {
    val a = eval(xs, kernelType)
    val t = this.threshold.getOrElse(threshold)
    if ((maxWins && a > t) || (!maxWins && a < t)) targetCategory.get else alternateTargetCategory.get
  }
}

/**
 * Used to store the support vector coefficients αi and b.
 *
 * @param coefficients
 * @param absoluteValue Contains the value of the absolute coefficient b.
 */
class Coefficients(val coefficients: Array[Coefficient], val absoluteValue: Double = 0.0) extends PmmlElement

/**
 * Coefficient αi is described
 */
class Coefficient(val value: Double = 0.0) extends PmmlElement

/**
 * Contains all support vectors required for the respective SVM instance.
 */
class SupportVectors(val supportVectors: Array[SupportVector]) extends PmmlElement

/**
 * SupportVector which only has the attribute vectorId - the reference to the support vector in VectorDictionary.
 */
class SupportVector(val vectorId: String) extends PmmlElement

/**
 * Usually the SVM model uses support vectors to define the model function. However, for the case of a linear function
 * (linear kernel type) the function is a linear hyperplane that can be more efficiently expressed using the
 * coefficients of all mining fields. In this case, no support vectors are required at all, and hence SupportVectors
 * will be absent and only the Coefficients element is necessary.
 *
 * The SVM representation specifies which of both representations is used:
 */
object SVMRepresentation extends Enumeration {
  type SVMRepresentation = Value
  val SupportVectors, Coefficients = Value
}

/**
 * The two most popular methods for multi-class classification are one-against-all (also known as one-against-rest)
 * and one-against-one. Depending on the method used, the number of SVMs built will differ.
 *
 * The SVM classification method specifies which of both methods is used:
 */
object SVMClassificationMethod extends Enumeration {
  type SVMClassificationMethod = Value
  val OneAgainstAll, OneAgainstOne = Value
}

trait HasSupportVectorMachineAttributes extends HasModelAttributes {

  /**
   * Defines whether the SVM function is defined via support vectors or via the coefficients of the hyperplane for the
   * case of linear kernel functions.
   */
  def svmRepresentation: SVMRepresentation = SVMRepresentation.SupportVectors

  /**
   * Defines which method is to be used in case of multi-class classification tasks. It can be either OneAgainstAll or
   * OneAgainstOne. This attribute is not required for binary classification or regression.
   */
  def classificationMethod: SVMClassificationMethod = SVMClassificationMethod.OneAgainstAll

  /**
   * Used for classification models only. It determines if the target category corresponding to the highest value of a
   * Support Vector machine is the winner. Default value is false, meaning the target category with the lowest SVM
   * value wins, consistent with previous PMML versions. This attribute also affects the comparisons with threshold
   * value, see below for details.
   */
  def maxWins: Boolean = false

  /**
   * Defines a discrimination boundary to be used in case of binary classification or whenever attribute
   * classificationMethod is defined as OneAgainstOne for multi-class classification tasks.
   */
  def threshold: Double = 0.0
}

trait HasWrappedSupportVectorMachineAttributes extends HasWrappedModelAttributes with HasSupportVectorMachineAttributes {

  override def attributes: SupportVectorMachineAttributes

  override def svmRepresentation: SVMRepresentation = attributes.svmRepresentation

  override def classificationMethod: SVMClassificationMethod = attributes.classificationMethod

  override def maxWins: Boolean = attributes.maxWins

  override def threshold: Double = attributes.threshold
}

class SupportVectorMachineAttributes(
                                      override val functionName: MiningFunction,
                                      override val threshold: Double = 0.0,
                                      override val svmRepresentation: SVMRepresentation = SVMRepresentation.SupportVectors,
                                      override val classificationMethod: SVMClassificationMethod = SVMClassificationMethod.OneAgainstAll,
                                      override val maxWins: Boolean = false,
                                      override val modelName: Option[String] = None,
                                      override val algorithmName: Option[String] = None,
                                      override val isScorable: Boolean = true
                                    ) extends ModelAttributes(functionName, modelName, algorithmName, isScorable)
  with HasSupportVectorMachineAttributes

class SVMOutputs extends MixedClsWithRegOutputs
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
package org.pmml4s.common

import org.pmml4s.common.MiningFunction.MiningFunction
import org.pmml4s.model.Model
import org.pmml4s.util.StringUtils

import scala.collection.immutable

trait HasVersion {
  self: Model =>

  /** PMML version. */
  def version: String = parent.version

  /** Returns PMML version as a double value */
  def dVersion: Double = StringUtils.asDouble(version)
}

case class Extension(extender: Option[String], name: Option[String], value: Option[Any], content: Option[Any])
  extends Serializable

/**
 * The PMML schema contains a mechanism for extending the content of a model. Extension elements should be present as
 * the first child in all elements and groups defined in PMML. This way it is possible to place information in the
 * Extension elements which affects how the remaining entries are treated. The main element in each model should have
 * Extension elements as the first and the last child for maximum flexibility.
 */
trait HasExtensions {
  def extensions: immutable.Seq[Extension] = immutable.Seq.empty

  def hasExtensions: Boolean = extensions.nonEmpty
}

/** The base trait for all elements of PMML */
trait PmmlElement extends HasExtensions with Serializable

/**
 * Describes the software application that generated the model.
 *
 * @param name    The name of the application that generated the model.
 * @param version The version of the application that generated this model.
 */
class Application(
                   val name: String,
                   val version: Option[String]) extends PmmlElement {
  override def toString: String = if (version.isDefined) (name + " " + version.get) else name
}

/**
 *
 * @param copyright
 * @param description
 * @param modelVersion
 * @param application
 */
class Header(
              val copyright: Option[String] = None,
              val description: Option[String] = None,
              val modelVersion: Option[String] = None,
              val application: Option[Application] = None) extends PmmlElement

object MiningFunction extends Enumeration {
  type MiningFunction = Value
  val associationRules, sequences, classification, regression, clustering, timeSeries, mixed = Value
}

/**
 * Holds common attributes of a PMML model.
 */
trait HasModelAttributes {
  /**
   * Identifies the model with a unique name in the context of the PMML file.
   * This attribute is not required. Consumers of PMML models are free to manage the names of the models at their discretion.
   */
  def modelName: Option[String]

  /**
   * Describe the kind of mining model, e.g., whether it is intended to be used for clustering or for classification.
   */
  def functionName: MiningFunction

  /**
   * The algorithm name is free-type and can be any description for the specific algorithm that produced the model.
   * This attribute is for information only.
   */
  def algorithmName: Option[String]

  /**
   * Indicates if the model is valid for scoring. If this attribute is true or if it is missing,
   * then the model should be processed normally. However, if the attribute is false,
   * then the model producer has indicated that this model is intended for information purposes only and should not be used to generate results.
   */
  def isScorable: Boolean

  /** Tests if this is a classification model. */
  def isClassification: Boolean = functionName == MiningFunction.classification

  /** Tests if this is a regression model. */
  def isRegression: Boolean = functionName == MiningFunction.regression

  /** Tests if this is a clustering model. */
  def isClustering: Boolean = functionName == MiningFunction.clustering

  /** Tests if this is a association rules model. */
  def isAssociationRules: Boolean = functionName == MiningFunction.associationRules

  /** Tests if this is a sequences model. */
  def isSequences: Boolean = functionName == MiningFunction.sequences

  /** Tests if this is a time series model. */
  def isTimeSeries: Boolean = functionName == MiningFunction.timeSeries

  /** Tests if this is a mixed model. */
  def isMixed: Boolean = functionName == MiningFunction.mixed
}

/**
 * Class represents common attributes of a PMML model.
 */
class ModelAttributes(
                       override val functionName: MiningFunction,
                       override val modelName: Option[String] = None,
                       override val algorithmName: Option[String] = None,
                       override val isScorable: Boolean = true) extends HasModelAttributes with Serializable

trait HasWrappedModelAttributes extends HasModelAttributes {
  /** Common attributes of this model */
  def attributes: ModelAttributes

  def modelName: Option[String] = attributes.modelName

  def functionName: MiningFunction = attributes.functionName

  def algorithmName: Option[String] = attributes.algorithmName

  def isScorable: Boolean = attributes.isScorable
}

trait HasParent {
  self: Model =>

  /** The parent model. */
  var parent: Model

  def setParent(parent: Model): this.type = {
    this.parent = parent
    this
  }
}

object ArrayType extends Enumeration {
  type ArrayType = Value
  val int, real, string = Value
}
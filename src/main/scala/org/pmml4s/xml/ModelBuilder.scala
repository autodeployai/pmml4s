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

import org.pmml4s.common._
import org.pmml4s.metadata.{DataDictionary, DataField, Field}
import org.pmml4s.model.{DataModel, Model}
import org.pmml4s.transformations.TransformationDictionary
import org.pmml4s.xml.XmlImplicits._
import org.pmml4s.{ElementNotFoundException, NotSupportedException, PmmlException, metadata}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.xml.MetaData
import scala.xml.pull.{EvElemEnd, EvElemStart, XMLEventReader}

/**
 * Base builder of PMML model
 */
class ModelBuilder extends TransformationsBuilder
  with ElemBuilder[Model] {
  private var version: String = _
  private var header: Header = _
  private var dataDict: DataDictionary = _
  private var transDict: TransformationDictionary = _
  private var extensions = ArrayBuffer.empty[Extension]

  def build(reader: XMLEventReader, attrs: XmlAttrs): Model = {
    version = attrs(AttrTags.VERSION)
    makeModel(reader)
  }

  private def makeModel(reader: XMLEventReader): Model = {
    while (reader.hasNext) {
      reader.next match {
        case EvElemStart(_, ElemTags.HEADER, attrs, _)                       => {
          header = makeHeader(reader, attrs)
        }
        case EvElemStart(_, ElemTags.MINING_BUILD_TASK, _, _)                => {
          handleElem(reader, ElemTags.MINING_BUILD_TASK)
        }
        case EvElemStart(_, ElemTags.DATA_DICTIONARY, attrs, _)              => {
          dataDict = makeDataDictionary(reader, attrs)
        }
        case EvElemStart(_, ElemTags.TRANSFORMATION_DICTIONARY, _, _)        => {
          transDict = makeTransformationDictionary(reader)
        }
        case EvElemStart(_, label, attrs, _) if ModelBuilder.contains(label) => {
          validate()
          val parent = new DataModel(version, header, dataDict, Option(transDict))
          val builder = Builder.get(label).getOrElse(throw new NotSupportedException(label))
          val model = builder.build(reader, attrs, parent)
          if (dataDict.exists(_.isMutable)) {
            dataDict.foreach(_.toImmutable())
          }
          builder.postBuild()

          return model
        }
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _)                    => {
          extHandler(reader, attrs).foreach(extensions += _)
        }
        case EvElemEnd(_, ElemTags.PMML)                                     => {
          validate()
          val parent = new DataModel(version, header, dataDict, Option(transDict))
          return if (parent.transformationDictionary.isDefined) parent.asTransformation else parent
        }
        case _                                                               =>
      }
    }

    throw new PmmlException("Not a valid PMML")
  }

  private def makeHeader(reader: XMLEventReader, attrs: MetaData): Header =
    makeElem(reader, attrs, new ElemBuilder[Header] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): Header = {
        val values = attrs.get(AttrTags.COPYRIGHT, AttrTags.DESCRIPTION, AttrTags.MODEL_VERSION)
        val app: Option[Application] = makeElem(reader, ElemTags.HEADER, ElemTags.APPLICATION, new ElemBuilder[Application] {
          def build(reader: XMLEventReader, attrs: XmlAttrs): Application =
            new Application(attrs(AttrTags.NAME), attrs.get(AttrTags.VERSION))
        })

        new Header(values._1, values._2, values._3, app)
      }
    })

  private def makeDataDictionary(reader: XMLEventReader, attrs: MetaData): DataDictionary = {
    val fields = makeElems(reader, ElemTags.DATA_DICTIONARY, ElemTags.DATA_FIELD, new ElemBuilder[DataField] {
      def build(reader: XMLEventReader, attrs: XmlAttrs): DataField = {
        val name = attrs(AttrTags.NAME)
        val opType = OpType.withName(attrs(AttrTags.OPTYPE))
        val dataType = attrs.get(AttrTags.DATA_TYPE).map { x => DataType.withName(x) } getOrElse (UnresolvedDataType)
        val displayName = attrs.get(AttrTags.DISPLAY_NAME)
        val intervals = new ArrayBuffer[Interval]
        val values = new ArrayBuffer[Value]

        traverseElems(reader, ElemTags.DATA_FIELD, {
          case EvElemStart(_, ElemTags.INTERVAL, attrs, _) => intervals += makeInterval(reader, attrs)
          case EvElemStart(_, ElemTags.VALUE, attrs, _)    => values += makeValue(reader, attrs)
          case _                                           =>
        })

        new DataField(name, displayName, dataType, opType, intervals, values)
      }
    })

    metadata.DataDictionary(fields)
  }

  def validate(): Unit = {
    if (header == null) throw new ElementNotFoundException(ElemTags.HEADER)
    if (dataDict == null) throw new ElementNotFoundException(ElemTags.DATA_DICTIONARY)
  }

  /**
   * Returns the field of a given name, None if a field with the given name does not exist
   */
  override def getField(name: String): Option[Field] = {
    val res = if (dataDict != null) dataDict.get(name) else None
    res orElse super.getField(name)
  }
}

object ModelBuilder extends ExtensionHandler with XmlUtils {

  /**
   * @param src
   * @return A built model ready to score an input data if successful, a PmmlExcpetion is thrown otherwise
   */
  def fromXml(src: Source): Model = {
    val reader = new XMLEventReader(src)
    while (reader.hasNext) {
      reader.next match {
        case event: EvElemStart => if (event.label == ElemTags.PMML) {
          return makeElem(reader, event, new ModelBuilder)
        } else throw new PmmlException("A PMML document is an XML document with a root element of type PMML")
        case _                  =>
      }
    }

    throw new PmmlException("Not a valid PMML")
  }

  /**
   * @param reader
   * @param label
   * @param attrs
   * @param parent
   * @return
   */
  def fromXml(reader: XMLEventReader, label: String, attrs: XmlAttrs, parent: Model): Model = {
    Builder.get(label).getOrElse(throw new NotSupportedException(label)).build(reader, attrs, parent)
  }

  /** List of models supported by the PMML 4.3: [[http://dmg.org/pmml/v4-3/GeneralStructure.html PMML 4.3 - General Structure]] */
  val PMML_SUPPORTED_MODELS = Set(
    ElemTags.ASSOCIATION_MODEL,
    ElemTags.BAYESIAN_NETWORK_MODEL,
    ElemTags.BASELINE_MODEL,
    ElemTags.CLUSTERING_MODEL,
    ElemTags.GAUSSIAN_PROCESS_MODEL,
    ElemTags.GENERAL_REGRESSION_MODEL,
    ElemTags.MINING_MODEL,
    ElemTags.NAIVE_BAYES_MODEL,
    ElemTags.NEAREST_NEIGHBOR_MODEL,
    ElemTags.NEURAL_NETWORK,
    ElemTags.REGRESSION_MODEL,
    ElemTags.RULE_SET_MODEL,
    ElemTags.SEQUENCE_MODEL,
    ElemTags.SCORECARD,
    ElemTags.SUPPORT_VECTOR_MACHINE_MODEL,
    ElemTags.TEXT_MODEL,
    ElemTags.TIME_SERIES_MODEL,
    ElemTags.TREE_MODEL)

  def contains(s: String): Boolean = {
    PMML_SUPPORTED_MODELS.contains(s)
  }
}
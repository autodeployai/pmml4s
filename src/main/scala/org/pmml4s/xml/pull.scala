/*
 * Copyright (c) 2023 AutoDeployAI
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

import javax.xml.stream.events.{Characters, Comment, EndElement, EntityReference, ProcessingInstruction, StartElement}
import javax.xml.stream.{EventFilter, XMLInputFactory, XMLStreamConstants}
import scala.io.Source
import XmlImplicits._

/**
 * Borrow codes from scala.xml.pull, and reimplement based on [[javax.xml.stream.XMLEventReader]]
 */
trait XMLEvent

/**
 * An Element's start tag was encountered.
 * @param pre prefix, if any, on the element.  This is the `xs` in `<xs:string>foo</xs:string>`.
 * @param label the name of the element, not including the prefix
 * @param attrs any attributes on the element
 */
case class EvElemStart(pre: String, label: String, attrs: XmlAttrs, scope: NamespaceBinding) extends XMLEvent

/**
 * An Element's end tag was encountered.
 * @param pre prefix, if any, on the element.  This is the `xs` in `<xs:string>foo</xs:string>`.
 * @param label the name of the element, not including the prefix
 */
case class EvElemEnd(pre: String, label: String) extends XMLEvent

/**
 * A text node was encountered.
 * @param text the text that was found
 */
case class EvText(text: String) extends XMLEvent

/**
 * An entity reference was encountered.
 * @param entity the name of the entity, e.g. `gt` when encountering the entity `&gt;`
 */
case class EvEntityRef(entity: String) extends XMLEvent

/**
 * A processing instruction was encountered.
 * @param target the "PITarget" of the processing instruction.  For the instruction `<?foo bar="baz"?>`, the target would
 * be `foo`
 * @param text the remainder of the instruction.  For the instruction `<?foo bar="baz"?>`, the text would
 * be `bar="baz"`
 * @see [[http://www.w3.org/TR/REC-xml/#sec-pi]]
 */
case class EvProcInstr(target: String, text: String) extends XMLEvent

/**
 * A comment was encountered
 * @param text the text of the comment
 */
case class EvComment(text: String) extends XMLEvent

/**
 * The class `NamespaceBinding` represents namespace bindings and scopes.
 */
case class NamespaceBinding(prefix: String, uri: String, parent: NamespaceBinding)

/**
 * Main entry point into creating an event-based XML parser.  Treating this
 * as a [[scala.collection.Iterator]] will provide access to the generated events.
 * @param src A [[scala.io.Source]] for XML data to parse
 */
class XMLEventReader(src: Source)
  extends scala.collection.Iterator[XMLEvent] {

  private val reader = {
    val factory = XMLInputFactory.newFactory
    val reader = factory.createXMLEventReader(new SourceWrapper(src))
    factory.createFilteredReader(reader, new EventFilter {
      override def accept(ev: javax.xml.stream.events.XMLEvent): Boolean = {
        !ev.isStartDocument &&
          ev.getEventType != XMLStreamConstants.DTD && !(ev.isCharacters && ev.asCharacters.isIgnorableWhiteSpace)
      }
    })
  }

  override def hasNext: Boolean = reader.hasNext

  override def next(): XMLEvent = reader.nextEvent match {
    case ev: StartElement =>
      EvElemStart(ev.getName.getPrefix, ev.getName.getLocalPart, ev.getAttributes, null)
    case ev: EndElement =>
      EvElemEnd(ev.getName.getPrefix, ev.getName.getLocalPart)
    case ev: Characters =>
      EvText(ev.getData)
    case ev: EntityReference =>
      EvEntityRef(ev.getName)
    case ev: ProcessingInstruction =>
      EvProcInstr(ev.getTarget, ev.getData)
    case ev: Comment =>
      EvComment(ev.getText)
    }
}

class SourceWrapper(src: Source) extends java.io.Reader {
  override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
    var i = off
    val end = off + math.min(len, cbuf.length - off)
    while (i < end && src.hasNext) {
      cbuf(i) = src.next()
      i += 1
    }
    val result = i - off
    if (result == 0 && !src.hasNext) {
      -1
    } else {
      result
    }
  }

  override def close(): Unit = src.close()
}

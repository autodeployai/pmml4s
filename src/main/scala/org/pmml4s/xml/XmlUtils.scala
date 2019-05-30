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

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.xml.MetaData
import scala.xml.pull._

trait XmlBase {
  def skipLabel(reader: XMLEventReader): Unit = {
    var i = 1
    while (i > 0) {
      nextLabel(reader) match {
        case _: EvElemStart => i += 1
        case _: EvElemEnd   => i -= 1
        case _              =>
      }
    }
  }

  def nextLabel(reader: XMLEventReader): XMLEvent = {
    while (reader.hasNext) {
      reader.next match {
        case start: EvElemStart => return start
        case end: EvElemEnd     => return end
        case _                  =>
      }
    }
    new XMLEvent {}
  }
}

trait XmlUtils extends XmlBase {
  self: ExtensionHandler =>

  def traverseElems(reader: XMLEventReader, parent: String,
                    f: PartialFunction[XMLEvent, Any],
                    handleText: Boolean = false,
                    handleEnd: Boolean = false,
                    handleEntityRef: Boolean = false,
                    handleExtension: Boolean = false): Any = {
    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if event.label == ElemTags.EXTENSION =>
          if (handleExtension) f(event) else skipLabel(reader)
        case event: EvElemStart                                      => f(event)
        case event: EvText if handleText                             => f(event)
        case event: EvEntityRef if handleEntityRef                   => f(event)
        case EvElemEnd(_, `parent`)                                  => done = true
        case event: EvElemEnd if handleEnd                           => f(event)
        case _                                                       =>
      }
    }
  }

  def makeElem[T](reader: XMLEventReader, attrs: MetaData, builder: ElemBuilder[T]): T = builder.build(reader, XmlAttrs(attrs))

  def makeElem[T](reader: XMLEventReader, event: EvElemStart, builder: ElemBuilder[T]): T = makeElem(reader, event.attrs, builder)

  def makeElem[T](reader: XMLEventReader, parent: String, child: String, builder: ElemBuilder[T]): Option[T] = {
    var result: Option[T] = None

    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if event.label == child   => result = Some(builder.build(reader, XmlAttrs(event.attrs)))
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _) => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                       => done = true
        case _                                            =>
      }
    }

    result
  }

  def makeElem[A, B](reader: XMLEventReader, parent: String,
                     child1: String, builder1: ElemBuilder[A],
                     child2: String, builder2: ElemBuilder[B]): (Option[A], Option[B]) = {
    var res1: Option[A] = None
    var res2: Option[B] = None

    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if event.label == child1  => res1 = Option(builder1.build(reader, XmlAttrs(event.attrs)))
        case event: EvElemStart if event.label == child2  => res2 = Option(builder2.build(reader, XmlAttrs(event.attrs)))
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _) => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                       => done = true
        case _                                            =>
      }
    }

    (res1, res2)
  }

  def makeElem[T](reader: XMLEventReader, event: EvElemStart, builder: GroupElemBuilder[T]): T = builder.build(reader, event)

  def makeElems[T: ClassTag](reader: XMLEventReader,
                             parent: String,
                             child: String,
                             builder: ElemBuilder[T],
                             sizeHint: Option[Int] = None): Array[T] = {
    val res = mutable.ArrayBuilder.make[T]()
    sizeHint.foreach(res.sizeHint(_))
    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if event.label == child   => res += builder.build(reader, XmlAttrs(event.attrs))
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _) => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                       => done = true
        case _                                            =>
      }
    }

    res.result()
  }

  def makeElems[A: ClassTag, B: ClassTag](reader: XMLEventReader, parent: String,
                                          child1: String, builder1: ElemBuilder[A],
                                          child2: String, builder2: ElemBuilder[B]): (Array[A], Array[B]) = {
    val res1 = mutable.ArrayBuilder.make[A]()
    val res2 = mutable.ArrayBuilder.make[B]()
    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if event.label == child1  => res1 += builder1.build(reader, XmlAttrs(event.attrs))
        case event: EvElemStart if event.label == child2  => res2 += builder2.build(reader, XmlAttrs(event.attrs))
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _) => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                       => done = true
        case _                                            =>
      }
    }

    (res1.result(), res2.result())
  }

  def makeElems[T: ClassTag](reader: XMLEventReader, parent: String, children: Set[String], builder: GroupElemBuilder[T]): Array[T] = {
    val res = mutable.ArrayBuilder.make[T]()
    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case event: EvElemStart if children.contains(event.label) => res += builder.build(reader, event)
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _)         => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                               => done = true
        case _                                                    =>
      }
    }

    res.result()
  }

  def extractText(reader: XMLEventReader, parent: String): String = {
    val res = new mutable.StringBuilder()
    var done = false
    while (!done && reader.hasNext) {
      reader.next match {
        case EvText(text)                                 => res ++= text
        case EvElemStart(_, ElemTags.EXTENSION, attrs, _) => extHandler(reader, attrs)
        case EvElemEnd(_, `parent`)                       => done = true
        case _                                            =>
      }
    }
    res.result()
  }

}

trait UnknownElemHandler extends XmlBase {

  def handleElem(reader: XMLEventReader, label: String, attrs: MetaData = scala.xml.Null): Unit = {
    skipLabel(reader)
  }

  def handleElem(reader: XMLEventReader, event: EvElemStart): Unit = handleElem(reader, event.label, event.attrs)
}
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
import org.pmml4s.metadata.Field
import org.pmml4s.util.Utils
import org.pmml4s.{InvalidValueException, PmmlException}

import scala.xml.pull.XMLEventReader

/**
 * Provides utilities to parse these common elements.
 */
trait CommonBuilder extends ExtensionHandler with UnknownElemHandler {

  def toVal(s: String, dataType: DataType): Any = Utils.toVal(s, dataType)

  /**
   * Verifies if the input sting values is valid for the specified field, here, we need to check if the field is null,
   * because this method is always called to verify the target field that could be absent for any model, for example
   * the child model under the Mining Model.
   */
  def verifyValue(s: String, f: Field): Any = {
    if (f != null) {
      val value = f.toVal(s)
      if (Utils.isMissing(f.encode(value))) throw new InvalidValueException(s, f.name) else value
    } else {
      s
    }
  }

  def makeValue(reader: XMLEventReader, attrs: XmlAttrs): Value = {
    val value = attrs(AttrTags.VALUE)
    val displayValue = attrs.get(AttrTags.DISPLAY_NAME)
    val property = attrs.get(AttrTags.PROPERTY).map(Property.withName(_)).getOrElse(Property.valid)

    new Value(value, displayValue, property)
  }


  def makeInterval(reader: XMLEventReader, attrs: XmlAttrs): Interval = {
    val closure = Closure.withName(attrs(AttrTags.CLOSURE))
    (attrs.get(AttrTags.LEFT_MARGIN, AttrTags.RIGHT_MARGIN) match {
      case (Some(left), Some(right)) => Interval(left.toDouble, right.toDouble, closure)
      case (Some(left), None)        => if (Closure.isOpenBelow(closure)) Interval.above(left.toDouble) else
        Interval.atOrAbove(left.toDouble)
      case (None, Some(right))       => if (Closure.isOpenAbove(closure)) Interval.below(right.toDouble) else
        Interval.atOrBelow(right.toDouble)
      case _                         => Interval()
    })
  }

  /** `??????` can be used for marking methods that never invoked
   *
   * @throws PmmlException
   */
  def ?????? : Nothing = throw new PmmlException("Never happen exception")
}

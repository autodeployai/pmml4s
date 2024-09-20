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

import org.pmml4s.common._
import org.pmml4s.data.DataVal
import org.pmml4s.metadata.Field
import org.pmml4s.util.{StringUtils, Utils}
import org.pmml4s.{InvalidValueException, PmmlException}


/**
 * Provides utilities to parse these common elements.
 */
trait CommonBuilder extends ExtensionHandler with UnknownElemHandler {

  def toVal(s: String, dataType: DataType): Any = Utils.toVal(s, dataType)
  def toDataVal(s: String, dataType: DataType): DataVal = Utils.toDataVal(s, dataType)

  /**
   * Verifies if the input sting values is valid for the specified field, here, we need to check if the field is null,
   * because this method is always called to verify the target field that could be absent for any model, for example
   * the child model under the Mining Model.
   */
  def verifyValue(s: String, f: Field): DataVal = {
    if (f != null) {
      val value = f.toVal(s)
      if (Utils.isMissing(f.encode(value))) throw new InvalidValueException(s, f.name) else value
    } else {
      DataVal.from(s)
    }
  }

  def makeValue(reader: XMLEventReader, attrs: XmlAttrs): Value = {
    val value = attrs(AttrTags.VALUE)
    val displayValue = attrs.get(AttrTags.DISPLAY_NAME)
    val property = attrs.get(AttrTags.PROPERTY).map(Property.withName).getOrElse(Property.valid)

    new Value(value, displayValue, property)
  }


  def makeInterval(reader: XMLEventReader, attrs: XmlAttrs): Interval = {
    val closure = Closure.withName(attrs(AttrTags.CLOSURE))
    (attrs.get(AttrTags.LEFT_MARGIN, AttrTags.RIGHT_MARGIN) match {
      case (Some(left), Some(right)) => Interval(StringUtils.asDouble(left), StringUtils.asDouble(right), closure)
      case (Some(left), None)        => if (Closure.isOpenBelow(closure)) Interval.above(StringUtils.asDouble(left)) else
        Interval.atOrAbove(StringUtils.asDouble(left))
      case (None, Some(right))       => if (Closure.isOpenAbove(closure)) Interval.below(StringUtils.asDouble(right)) else
        Interval.atOrBelow(StringUtils.asDouble(right))
      case _                         => Interval()
    })
  }

  /** `??????` can be used for marking methods that never invoked
   *
   * @throws PmmlException
   */
  def ?????? : Nothing = throw new PmmlException("Never happen exception")
}


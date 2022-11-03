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

import org.pmml4s.AttributeNotFoundException
import org.pmml4s.util.StringUtils

import scala.xml._

case class XmlAttrs(attrs: Map[String, String]) {

  def has(name: String): Boolean = attrs.contains(name)

  def apply(name: String): String = attrs.getOrElse(name, throw new AttributeNotFoundException(name))

  def apply(n1: String, n2: String): (String, String) = (apply(n1), apply(n2))

  def apply(n1: String, n2: String, n3: String): (String, String, String) = (apply(n1), apply(n2), apply(n3))

  def double(name: String): Double = apply(name).toDouble

  def int(name: String): Int = apply(name).toInt

  def `enum`(name: String, e: Enumeration) = e.withName(apply(name))

  def getInt(name: String): Option[Int] = get(name).flatMap { x => StringUtils.toInt(x) }

  def getInt(name: String, d: Int): Int = getInt(name).getOrElse(d)

  def getLong(name: String): Option[Long] = get(name).flatMap { x => StringUtils.toLong(x) }

  def getLong(name: String, d: Long): Long = getLong(name).getOrElse(d)

  def getDouble(name: String): Option[Double] = get(name).flatMap { x => StringUtils.toDouble(x) }

  def getDouble(name: String, d: Double): Double = getDouble(name).getOrElse(d)

  def getBoolean(name: String): Option[Boolean] = get(name).flatMap { x => StringUtils.toBool(x) }

  def getBoolean(name: String, d: Boolean): Boolean = getBoolean(name).getOrElse(d)

  def getString(name: String, d: String): String = get(name).getOrElse(d)

  def getEnum(name: String, e: Enumeration) = get(name).map(e.withName(_))

  def get(name: String): Option[String] = attrs.get(name)

  def -(name: String) = XmlAttrs(attrs - name)

  def +(name: String, value: String) = XmlAttrs(attrs + ((name, value)))

  def toMetadata: MetaData = ((Null: MetaData) /: attrs) ((a, e) => a append new UnprefixedAttribute(e._1, e._2, Null))

  def get(n1: String, n2: String): (Option[String], Option[String]) = (get(n1), get(n2))

  def get(n1: String, n2: String, n3: String): (Option[String], Option[String], Option[String]) = (get(n1), get(n2), get(n3))

  def get(n1: String, n2: String, n3: String, n4: String): (Option[String], Option[String], Option[String], Option[String]) =
    (get(n1), get(n2), get(n3), get(n4))

  def get(n1: String, n2: String, n3: String, n4: String, n5: String): (Option[String], Option[String], Option[String], Option[String], Option[String]) =
    (get(n1), get(n2), get(n3), get(n4), get(n5))

  def get(n1: String, n2: String, n3: String, n4: String, n5: String, n6: String): (Option[String], Option[String], Option[String], Option[String], Option[String], Option[String]) =
    (get(n1), get(n2), get(n3), get(n4), get(n5), get(n6))

  def getDouble(n1: String, n2: String): (Option[Double], Option[Double]) = (getDouble(n1), getDouble(n2))
}

object XmlAttrs {
  def apply(m: MetaData) = XmlImplicits.metaData2Attr(m)

  def apply(name: String, value: String) = new XmlAttrs(Map(name -> value))

  def apply() = new XmlAttrs(Map.empty)
}

object XmlImplicits {
  implicit def metaData2Attr(attrs: MetaData): XmlAttrs = XmlAttrs(attrs.asAttrMap)

  implicit def attrs2MetaData(attributes: XmlAttrs): MetaData =
    ((Null: MetaData) /: attributes.attrs) ((acc, attr) => new UnprefixedAttribute(attr._1, attr._2, acc))
}

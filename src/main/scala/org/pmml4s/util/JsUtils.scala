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
package org.pmml4s.util

import spray.json.{JsFalse, JsNull, JsNumber, JsString, JsTrue, JsValue, JsonFormat}

/**
 * Various utility methods about spray-json
 */
object JsUtils {

  implicit object AnyJsonFormat extends JsonFormat[Any] {
    def write(x: Any) = x match {
      case n: Int    => JsNumber(n)
      case n: Double => JsNumber(n)
      case n: Long   => JsNumber(n)
      case n: Short  => JsNumber(n)
      case n: Byte   => JsNumber(n)
      case s: String => JsString(s)
      case true      => JsTrue
      case false     => JsFalse
      case _         => JsNull
    }

    def read(value: JsValue) = value match {
      case JsNumber(n) => n
      case JsString(s) => s
      case JsTrue      => true
      case JsFalse     => false
      case JsNull      => null
      case _           => null
    }
  }

  def toJsValue(x: Any): JsValue = x match {
    case n: Int    => JsNumber(n)
    case n: Double => JsNumber(n)
    case n: Long   => JsNumber(n)
    case n: Short  => JsNumber(n)
    case n: Byte   => JsNumber(n)
    case s: String => JsString(s)
    case true      => JsTrue
    case false     => JsFalse
    case _         => JsNull
  }

}

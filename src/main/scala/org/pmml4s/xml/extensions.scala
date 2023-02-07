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

import org.pmml4s.common.Extension


trait ExtensionHandler extends XmlUtils {
  def extHandler: (XMLEventReader, XmlAttrs) => Option[Extension] = {
    (x, _) => skipLabel(x); None
  }
}

trait SPSSExtensions extends ExtensionHandler {
  override def extHandler: (XMLEventReader, XmlAttrs) => Option[Extension] = {
    ???
  }
}

trait SASExtensions extends ExtensionHandler {
  override def extHandler: (XMLEventReader, XmlAttrs) => Option[Extension] = {
    ???
  }
}

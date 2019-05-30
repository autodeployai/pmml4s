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
package org.pmml4s

class PmmlException(message: String, cause: Throwable)
  extends Exception(message, cause) {

  def this(message: String) = this(message, null)
}

class AttributeNotFoundException(attr: String)
  extends PmmlException(s"Required attribute '$attr' is missing.")

class FieldNotFoundException(name: String)
  extends PmmlException(s"""Field '$name' does not exist in PMML scope.""")

class TargetNotFoundException(name: String)
  extends PmmlException(s"""Target does not exist PMML score of '$name'.""")

class ElementNotFoundException(name: String)
  extends PmmlException(s"Required element '${name}' is missing.")

class SemanticErrorException(message: String)
  extends PmmlException(message)

class NotSupportedException(name: String)
  extends PmmlException(s"The '$name' is not supported by PMML4S.")

class DataNotMatchedException(message: String)
  extends PmmlException(message)

class InvalidValueException(value: String, field: String)
  extends PmmlException(s"""'$value' is not valid for the field '$field'""")

class FunctionNotFoundException(name: String)
  extends PmmlException(s"Function '${name}' is not defined.")
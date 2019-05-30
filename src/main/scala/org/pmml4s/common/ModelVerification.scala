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

import org.pmml4s.model.Model

/**
 * Provides a dataset of model inputs and known results that can be used to verify accurate results are generated,
 * regardless of the environment.
 */
class ModelVerification extends PmmlElement {
  ???
}

trait HasModelVerification {
  self: Model =>
  def modelVerification: Option[ModelVerification]
}


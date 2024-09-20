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
package org.pmml4s.model

import org.pmml4s.data.{DataVal, Series}

/**
 * Neural network model cases come from DMG examples: http://dmg.org/pmml/v4-3/NeuralNetwork.html
 */
class NeuralNetworkTest extends BaseModelTest {

  test("Example model") {
    val model = Model.fromFile("src/test/resources/models/neural/dmg_neural_normal.xml")
    assert(model.modelElement === ModelElement.NeuralNetwork)
    assert(model.inputNames === Array("gender", "no of claims", "domicile", "age of car"))
    assert(model.targetName === "amount of claims")
    val r = model.predict(Series("  female", "       0", "suburban", 5))
    assert(r(0) === DataVal(3067.3031886618014))
  }

}

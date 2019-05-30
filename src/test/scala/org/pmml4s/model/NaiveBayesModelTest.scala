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

import org.pmml4s.data.Series

/**
 * Naive bayes model cases come from DMG examples: http://dmg.org/pmml/v4-3/NaiveBayes.html
 */
class NaiveBayesModelTest extends BaseModelTest {

  test("Scoring procedure") {
    val model = Model.fromFile("src/test/resources/models/naive/dmg_naive_bayes.xml")
    assert(model.modelElement === ModelElement.NaiveBayesModel)
    val nb = model.asInstanceOf[NaiveBayesModel]
    assert(model.inputNames === Array("age of individual", "gender", "no of claims", "domicile", "age of car"))
    assert(model.targetName === "amount of claims")
    val r = model.predict(Series(24, "male", "2", null, 1))
    assert(r(0) === 500)
    assert(r.getDouble(1) === 0.3577224807531526)
    assert(r.getDouble(2) === 0.04043221681936915)
    assert(r.getDouble(3) === 0.3577224807531526)
    assert(r.getDouble(4) === 0.26372180444146)
    assert(r.getDouble(5) === 0.026700193675882444)
    assert(r.getDouble(6) === 0.3114233043101357)
  }

}

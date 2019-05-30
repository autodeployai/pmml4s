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
 * SVM model cases come from DMG examples: http://dmg.org/pmml/v4-3/SupportVectorMachine.html
 */
class SupportVectorMachineModelTest extends BaseModelTest {

  test("Scoring with continuous predictors") {
    val model = Model.fromFile("src/test/resources/models/svm/dmg_svm_classification_cont_predictors.xml")
    assert(model.modelElement === ModelElement.SupportVectorMachineModel)
    assert(model.inputNames === Array("x1", "x2"))
    assert(model.targetName === "class")
    val r = model.predict(Series(0, 0))
    assert(r(0) === "no")

    val r2 = model.predict(Series(0, 1))
    assert(r2(0) === "yes")

    val r3 = model.predict(Series(1, 0))
    assert(r3(0) === "yes")

    val r4 = model.predict(Series(1, 1))
    assert(r4(0) === "no")
  }

  test("Scoring with a categorical predictor") {
    val model = Model.fromFile("src/test/resources/models/svm/dmg_svm_classification_cat_predictors.xml")
    assert(model.modelElement === ModelElement.SupportVectorMachineModel)
    assert(model.inputNames === Array("Age", "Employment"))
    assert(model.targetName === "TARGET")
    val r = model.predict(Series(20, "Private"))
    assert(r(0) === "1")
  }

}


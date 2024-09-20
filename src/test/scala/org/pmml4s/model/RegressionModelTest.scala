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
 * Regression model cases come from DMG examples: http://dmg.org/pmml/v4-3/Regression.html
 */
class RegressionModelTest extends BaseModelTest {

  test("Linear Regression Sample") {
    val model = Model.fromFile("src/test/resources/models/regression/dmg_regression_linear.xml")
    assert(model.modelElement === ModelElement.RegressionModel)
    val reg = model.asInstanceOf[RegressionModel]
    assert(reg.regressionTables.length === 1)
    assert(reg.regressionTables.head.targetCategory.isEmpty)
    assert(reg.regressionTables.head.predictors.length == 4)
    assert(model.inputNames === Array("age", "salary", "car_location"))
    assert(model.targetName === "number_of_claims")
    val r = model.predict(Array(20, 1000, "street"))
    assert(r(0) === 609.4)
  }

  test("Polynomial Regression Sample") {
    val model = Model.fromFile("src/test/resources/models/regression/dmg_regression_polynomial.xml")
    assert(model.modelElement === ModelElement.RegressionModel)
    val r = model.predict(Array(1000, "street"))
    assert(r(0) === 3426.0813300000004)
  }

  test("Logistic Regression for binary classification") {
    val model = Model.fromFile("src/test/resources/models/regression/dmg_regression_logistic.xml")
    assert(model.modelElement === ModelElement.RegressionModel)
    val r = model.predict(Array(1.0, 2.0))
    assert(r(0) === "no")
  }

  test("Sample for classification with more than two categories") {
    val model = Model.fromFile("src/test/resources/models/regression/dmg_regression_multi_classes.xml")
    assert(model.modelElement === ModelElement.RegressionModel)
    val r = model.predict(Array(20.0, 10.0, "0", 0))
    assert(r(0) === "professional")
  }

  test("Using interaction terms") {
    val model = Model.fromFile("src/test/resources/models/regression/dmg_regression_interaction.xml")
    assert(model.modelElement === ModelElement.RegressionModel)
    val r = model.predict(Array(20.0, 1.0, "male"))
    assert(r(0) === 0.10000000000000009)
  }
}

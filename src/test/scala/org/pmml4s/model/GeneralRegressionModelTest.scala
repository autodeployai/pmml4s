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

/**
 * General regression model cases come from DMG examples: http://dmg.org/pmml/v4-3/GeneralRegression.html
 */
class GeneralRegressionModelTest extends BaseModelTest {

  test("Multinomial Logistic Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_multinomial_logistic.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("minority", "sex", "age", "work"))
    assert(model.targetName === "jobcat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.multinomialLogistic)
    val r = model.predict(Map("sex" -> 1, "minority" -> 0, "age" -> 25, "work" -> 4))
    assert(r("predicted_jobcat") === 2.0)
  }

  test("General Linear Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_generalLinear.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("minority", "sex", "age", "work"))
    assert(model.targetName === "jobcat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.generalLinear)
    val r = model.predict(Map("sex" -> 1, "minority" -> 0, "age" -> 25, "work" -> 4))
    assert(r("predicted_jobcat") === 1.7710000000000001)
  }

  test("Ordinal Multinomial Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_ordinal_multinomial.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("minority", "sex", "age", "work"))
    assert(model.targetName === "jobcat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.ordinalMultinomial)
    val r = model.predict(Map("sex" -> 1, "minority" -> 0, "age" -> 25, "work" -> 4))
    assert(r("predicted_jobcat") === 2)
  }

  test("Simple Regression Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_regression.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("age", "work"))
    assert(model.targetName === "jobcat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.regression)
    val r = model.predict(Map("age" -> 25, "work" -> 4))
    assert(r("predicted_jobcat") === 2.2830000000000004)
  }

  test("Generalized Linear Model Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_generalizedLinear.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("minority", "sex", "age", "work"))
    assert(model.targetName === "jobcat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.generalizedLinear)
    val r = model.predict(Map("sex" -> 1, "minority" -> 0, "age" -> 25, "work" -> 4))
    assert(r("predicted_jobcat") === 1.7744268679597344)
  }

  test("Example of a model with contrast matrices") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_multinomial_contrast_matrix.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("gender", "educ", "jobcat", "salbegin"))
    assert(model.targetName === "salCat")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.multinomialLogistic)
    val r = model.predict(Map("gender" -> "f", "educ" -> 19, "jobcat" -> 3, "salbegin" -> 45000))
    assert(r("predicted_salCat") === "Low")
  }

  test("Cox Regression Model Example") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_cox.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("childs", "happy", "educ"))
    assert(model.targetName === "life")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.CoxRegression)
    val r = model.predict(Map("childs" -> 5, "happy" -> 1, "educ" -> 19))
    assert(r("predicted_life") === 0.9508934267989405)
  }

  test("Cox Regression Model Example with baselineStrataVariable") {
    val model = Model.fromFile("src/test/resources/models/general/dmg_general_cox_strata.xml")
    assert(model.modelElement === ModelElement.GeneralRegressionModel)
    assert(model.inputNames === Array("childs", "happy", "educ", "region"))
    assert(model.targetName === "life")
    val reg = model.asInstanceOf[GeneralRegressionModel]
    assert(reg.modelType === GeneralModelType.CoxRegression)
    val r = model.predict(Map("childs" -> 5, "happy" -> 1, "educ" -> 19, "region" -> 3))
    assert(r("predicted_life") === 0.9494936567021923)
  }
}

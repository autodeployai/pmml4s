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
 * Nearest neighbor model cases come from DMG examples: http://dmg.org/pmml/v4-3/KNN.html
 */
class NearestNeighborModelTest extends BaseModelTest {

  test("Scoring Example 1: Continuous predictors and two targets: continuous and categorical") {
    val model = Model.fromFile("src/test/resources/models/knn/dmg_knn_continuous_predictors_two_targets.xml")
    assert(model.modelElement === ModelElement.NearestNeighborModel)
    val knn = model.asInstanceOf[NearestNeighborModel]
    assert(model.inputNames === Array("petal length", "petal width", "sepal length", "sepal width"))
    assert(model.targetNames === Array("species", "species_class"))
    assert(model.isMixed === true)
    assert(model.isRegression("species") === true)
    assert(model.isClassification("species_class") === true)
    val r = model.predict("sepal length" -> 5.1, "sepal width" -> 3.5, "petal length" -> 1.4, "petal width" -> 0.2)
    assert(r.length === 2)
    assert(r(0)._2 === 10.0)
    assert(r(1)._2 === "Iris-setosa")
  }

  test("Scoring Example 2: Categorical predictor and categorical target with internal case ID variable") {
    val model = Model.fromFile("src/test/resources/models/knn/dmg_knn_categorical_predictor_categorical_target.xml")
    assert(model.modelElement === ModelElement.NearestNeighborModel)
    val knn = model.asInstanceOf[NearestNeighborModel]
    assert(model.inputNames === Array("marital status", "age", "dependents"))
    assert(model.targetNames === Array("income"))
    assert(model.isClassification === true)
    val r = model.predict("marital status" -> "s", "age" -> 20, "dependents" -> 2, "petal width" -> 0.2)
    assert(r.length === 4)
    assert(r(0)._2 === "Low")
    assert(r(1)._2 === "1")
    assert(r(2)._2 === "2")
    assert(r(3)._2 === "11")
  }

}

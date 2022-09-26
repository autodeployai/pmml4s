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
 * Mining model cases come from DMG examples: http://dmg.org/pmml/v4-3/MultipleModels.html
 */
class MiningModelTest extends BaseModelTest {

  test("an ensemble of classification trees whose results are combined by majority vote") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_majority_vote_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "day", "continent"))
    assert(model.targetName === "Class")
    assert(model.classes === Array("Iris-setosa", "Iris-versicolor", "Iris-virginica"))
    assert(model.field("continent").validValues === Array("africa", "asia"))
    val r = model.predict(Series(2.0, 1.75, 10, "africa"))
    assert(r(0) === "Iris-setosa")
    assert(r(1) === 1.0)
  }

  test("an ensemble of regression trees whose results are combined by weighted averaging") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_weighted_average_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "day", "continent", "sepal_width"))
    assert(model.targetName === "sepal_length")
    val r = model.predict(Series(2.0, 1.75, 10, "africa", 1.0))
    assert(r(0) === 5.254244999999999)
  }

  test("the first for which the predicate element of a segment is satisfied") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_select_first_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "day", "continent"))
    assert(model.targetName === "Class")
    val r = model.predict(Series(2.0, 1.75, 10, "africa"))
    assert(r(0) === "Iris-setosa")
    assert(r(1) === 1.0)
  }

  test("chain of models which output a \"Pollen Index\"") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_model_chain_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("Class", "PollenIndex"))
    val r = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r(0) === 1.5000000000000002)
  }

  test("the model composition approach where a decision tree model is used to select a regression model") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_model_composition_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("Class", "PollenIndex"))
    val r = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r(0) === 0.7)
  }

  test("a more efficient implementation of the model composition approach where a decision tree model is used to select a regression model") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_mining_efficient_model_chain.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("PollenIndex"))
    val r = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r(0) === 0.7)
  }

  test("an ensemble of three trees in which each tree is weighted based on the node assignment") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_weighted_majority_vote_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("SEPALLEN", "SEPALWID", "PETALLEN", "PETALWID"))
    assert(model.targetNames === Array("SPECIES"))
    val r = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r(2) === 0.5555556666666667)
    assert(r(3) === 3)
  }
}

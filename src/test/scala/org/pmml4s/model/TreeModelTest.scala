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

import org.pmml4s.common.MiningFunction
import org.pmml4s.metadata.ResultFeature

/**
 * Tree model cases come from DMG examples: http://dmg.org/pmml/v4-3/TreeModel.html
 */
class TreeModelTest extends BaseModelTest {

  test("Scoring Procedure") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_normal.xml")
    assert(model.modelElement === ModelElement.TreeModel)
    assert(model.modelName === Some("golfing"))
    assert(model.functionName === MiningFunction.classification)
    assert(model.inputNames === Array("temperature", "humidity", "windy", "outlook"))
    assert(model.targetName === "whatIdo")

    val outputs = model.outputFields
    assert(outputs.length === 1)
    assert(outputs(0).feature === ResultFeature.predictedValue)

    val r = model.predict("temperature" -> 75, "humidity" -> 55, "windy" -> "false", "outlook" -> "overcast")
    assert(r.size === 1)
    assert(r.head._2 === "may play")
  }

  test("Scoring Procedure in records json format") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_normal.xml")
    val r = model.predict("""[{"temperature": 75, "humidity": 55, "windy": "false", "outlook": "overcast"}]""")
    assert(r ==="""[{"predicted_whatIdo":"may play"}]""")
  }

  test("Scoring Procedure in split json format") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_normal.xml")
    val r = model.predict("""{"columns": ["temperature", "humidity", "windy", "outlook"], "data":[[75, 55, "false", "overcast"]]}""")
    assert(r ==="""{"columns":["predicted_whatIdo"],"data":[["may play"]]}""")
  }

  test("Scoring with residuals") {
    val model = Model.fromFile("src/test/resources/models/tree/sas_3.1_iris_tree.xml")
    val r = model.predict(Map("petal_width" -> 0.2, "petal_length" -> 1.4, "sepal_length" -> 5.1, "species" -> "IRIS-VIRGINICA"))
    assert(r("R_speciesIRIS_VIRGINICA ") === 1.0)

    val r2 = model.predict(Map("petal_width" -> 0.2, "petal_length" -> 1.4, "sepal_length" -> 5.1))
    assert(r2("R_speciesIRIS_VIRGINICA ") === null)
  }

  test("Scoring with explicit confidences") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_value_strategies.xml")
    val r = model.predict(Map("temperature" -> 45, "outlook" -> "sunny", "humidity" -> 60))
    assert(r.size === 7)
    assert(r("predicted_whatIdo") === "no play")
    assert(r("confidence") === 0.6)
  }

  test("Scoring with a missing value, and weightedConfidence missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_value_strategies.xml")
    val r = model.predict(Map("outlook" -> "sunny"))
    assert(r("predicted_whatIdo") === "will play")
    assert(r("confidence").asInstanceOf[Double] === 0.8)
  }

  test("Scoring with multiple missing values, and weightedConfidence missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_value_strategies.xml")
    val r = model.predict(Map.empty[String, Any])
    assert(r("predicted_whatIdo") === "will play")
    assert(r("confidence").asInstanceOf[Double] === 0.6)
  }

  test("Scoring with defaultChild missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_defaultChild.xml")
    val r = model.predict(Map("temperature" -> 40, "humidity" -> 70))
    assert(r("predicted_whatIdo") === "no play")
    assert(r("confidence") === 0.48)
  }

  test("Scoring with lastPredictedValue missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_lastPrediction.xml")
    val r = model.predict(Map("outlook" -> "sunny"))
    assert(r("predicted_whatIdo") === "will play")
    assert(r("confidence") === 0.8)
  }

  test("Scoring with nullPredictedValue missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_nullPredictedValue.xml")
    val r = model.predict(Map("outlook" -> "sunny"))
    assert(r("predicted_whatIdo") === null)
    assert(java.lang.Double.isNaN(r("confidence").asInstanceOf[Double]))
  }

  test("Scoring with aggregateNodes missing value handling") {
    val model = Model.fromFile("src/test/resources/models/tree/dmg_tree_missing_aggregateNodes.xml")
    val r = model.predict(Map("temperature" -> 45, "humidity" -> 90))
    assert(r("predicted_whatIdo") === "may play")
    assert(r("confidence") === 0.4666666666666667)
  }
}
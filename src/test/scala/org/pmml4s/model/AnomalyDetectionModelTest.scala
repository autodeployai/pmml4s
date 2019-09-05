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
 * Anomaly Detection Model cases come from DMG examples: http://dmg.org/pmml/v4-4/AnomalyDetectionModel.html
 */
class AnomalyDetectionModelTest extends BaseModelTest {

  test("An example of an isolation forest model") {
    val model = Model.fromFile("src/test/resources/models/anomaly/dmg_anomaly_iforest.xml")
    assert(model.modelElement === ModelElement.AnomalyDetectionModel)
    model.asInstanceOf[AnomalyDetectionModel]
    assert(model.inputNames === Array("sepal_length", "petal_length", "petal_width"))
    assert(model.outputNames === Array("anomalyScore", "anomaly"))
    val r = model.predict(Map("sepal_length" -> 1.5, "petal_length" -> 5.8, "petal_width" -> 4.6))
    assert(r("anomalyScore") === 0.40917716054246367)
    assert(r("anomaly") === true)
  }

  test("An example of an OCSVM model") {
    val model = Model.fromFile("src/test/resources/models/anomaly/dmg_anomaly_ocsvm.xml")
    assert(model.modelElement === ModelElement.AnomalyDetectionModel)
    model.asInstanceOf[AnomalyDetectionModel]
    assert(model.inputNames === Array("sepal_length", "sepal_width", "petal_length", "petal_width"))
    assert(model.outputNames === Array("anomalyScore", "anomaly"))
    val r = model.predict(Map("sepal_length" -> 0.0, "sepal_width" -> 0.5, "petal_length" -> 1.0, "petal_width" -> 2.0))
    assert(r("anomalyScore").asInstanceOf[Double] === -4.7832)
    assert(r("anomaly") === true)
  }

  test("An example of a Clustering-based model") {
    val model = Model.fromFile("src/test/resources/models/anomaly/dmg_anomaly_cluster.xml")
    assert(model.modelElement === ModelElement.AnomalyDetectionModel)
    model.asInstanceOf[AnomalyDetectionModel]
    assert(model.inputNames === Array("sepal_length", "sepal_width", "petal_length", "petal_width"))
    assert(model.outputNames === Array("anomalyScore", "anomaly"))
    val r = model.predict(Map("sepal_length" -> 5.1, "sepal_width" -> 3.5, "petal_length" -> 1.4, "petal_width" -> 0.2))
    assert(r("anomalyScore").asInstanceOf[Double] === 0.2908433613668061)
    assert(r("anomaly") === false)

    val r2 = model.predict(Map("sepal_length" -> 5.0, "sepal_width" -> 2.0, "petal_length" -> 3.5, "petal_width" -> 1.0))
    assert(r2("anomalyScore").asInstanceOf[Double] === 2.1761203019718547)
    assert(r2("anomaly") === true)

  }
}
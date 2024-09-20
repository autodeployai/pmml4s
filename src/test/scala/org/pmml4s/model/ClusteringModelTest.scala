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
 * Clustering model cases come from DMG examples: http://dmg.org/pmml/v4-3/ClusteringModel.html
 */
class ClusteringModelTest extends BaseModelTest {

  test("Example for a center-based clustering model") {
    val model = Model.fromFile("src/test/resources/models/cluster/dmg_clustering_center_based.xml")
    assert(model.modelElement === ModelElement.ClusteringModel)
    val clustering = model.asInstanceOf[ClusteringModel]
    assert(clustering.inputNames === Array("marital status", "age", "salary"))
    assert(clustering.targetNames.length === 0)
    assert(clustering.targetField === null)
    val r = model.predict(Array("s", 20, 5000))
    assert(r(0) === "2")
    assert(r(1) === "marital status is m")
    assert(r(2) === 471.587647006278)
  }

}
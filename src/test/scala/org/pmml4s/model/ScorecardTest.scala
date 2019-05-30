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
import org.pmml4s.data.Series
import org.pmml4s.metadata.ResultFeature

/**
 * Scorecard model cases come from DMG examples: http://dmg.org/pmml/v4-3/Scorecard.html
 */
class ScorecardTest extends BaseModelTest {

  test("Scoring Procedure") {
    val model = Model.fromFile("src/test/resources/models/scorecard/dmg_scorecard_example.xml")
    assert(model.modelElement === ModelElement.Scorecard)
    assert(model.modelName === Some("SampleScorecard"))
    assert(model.functionName === MiningFunction.regression)
    assert(model.inputNames === Array("department", "age", "income"))
    assert(model.targetName === "overallScore")

    val scorecard = model.asInstanceOf[Scorecard]
    assert(scorecard.useReasonCodes)
    assert(scorecard.reasonCodeAlgorithm === ReasonCodeAlgorithm.pointsBelow)
    assert(scorecard.initialScore === 0.0)
    assert(scorecard.baselineMethod === BaselineMethod.other)

    val outputFields = model.outputFields
    assert(outputFields.size === 4)
    assert(outputFields(0).feature === ResultFeature.predictedValue)
    assert(outputFields(1).feature === ResultFeature.reasonCode)
    assert(outputFields(1).rank === 1)
    assert(outputFields(2).feature === ResultFeature.reasonCode)
    assert(outputFields(2).rank === 2)
    assert(outputFields(3).feature === ResultFeature.reasonCode)
    assert(outputFields(3).rank === 3)

    val result = model.predict(Series("engineering", 25, 500))
    assert(result(0) === 29)
    assert(result(1) === "RC2")
    assert(result(2) === "RC1")
    assert(result(3) === "RC1")
  }
}
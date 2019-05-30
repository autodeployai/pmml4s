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
 * Ruleset model cases come from DMG examples: http://dmg.org/pmml/v4-3/RuleSet.html
 */
class RuleSetModelTest extends BaseModelTest {

  test("PMML for the example (using only simple rules)") {
    val model = Model.fromFile("src/test/resources/models/rule/dmg_rule_simple.xml")
    assert(model !== null)
    assert(model.modelElement === ModelElement.RuleSetModel)
    val rule = model.asInstanceOf[RuleSetModel]
    val r = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))

    // The default criterion="weightedSum" scoring
    assert(r("PredictedValue") === "drugA")
    assert(r("Confidence") === 0.32)

    // criterion="firstHit" scoring
    rule.criterion = Criterion.firstHit
    val r2 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r2("PredictedValue") === "drugB")
    assert(r2("Confidence") === 0.9)

    // criterion="weightedMax" scoring
    rule.criterion = Criterion.firstHit
    val r3 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r3("PredictedValue") === "drugB")
    assert(r3("Confidence") === 0.9)
  }

  test("PMML for the example (using compound rules)") {
    val model = Model.fromFile("src/test/resources/models/rule/dmg_rule_compound.xml")
    assert(model !== null)
    assert(model.modelElement === ModelElement.RuleSetModel)
    val rule = model.asInstanceOf[RuleSetModel]
    val r = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))

    // The default criterion="weightedSum" scoring
    assert(r("PredictedValue") === "drugA")
    assert(r("Confidence") === 0.32)

    // criterion="firstHit" scoring
    rule.criterion = Criterion.firstHit
    val r2 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r2("PredictedValue") === "drugB")
    assert(r2("Confidence") === 0.9)

    // criterion="weightedMax" scoring
    rule.criterion = Criterion.firstHit
    val r3 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r3("PredictedValue") === "drugB")
    assert(r3("Confidence") === 0.9)
  }
}

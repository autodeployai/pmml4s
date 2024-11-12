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
    assert(model.targetName === "$C-Drug")
    val rule = model.asInstanceOf[RuleSetModel]
    val r = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))

    // The default criterion="weightedSum" scoring
    assert(r("predicted_$C-Drug") === "drugA")
    assert(r("confidence") === 0.32)

    // criterion="firstHit" scoring
    rule.criterion = Criterion.firstHit
    val r2 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r2("predicted_$C-Drug") === "drugB")
    assert(r2("confidence") === 0.9)

    // criterion="weightedMax" scoring
    rule.criterion = Criterion.firstHit
    val r3 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r3("predicted_$C-Drug") === "drugB")
    assert(r3("confidence") === 0.9)
  }

  test("PMML for the example (using compound rules)") {
    val model = Model.fromFile("src/test/resources/models/rule/dmg_rule_compound.xml")
    assert(model !== null)
    assert(model.modelElement === ModelElement.RuleSetModel)
    assert(model.targetName === "$C-Drug")
    val rule = model.asInstanceOf[RuleSetModel]
    val r = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))

    // The default criterion="weightedSum" scoring
    assert(r("predicted_$C-Drug") === "drugA")
    assert(r("confidence") === 0.32)

    // criterion="firstHit" scoring
    rule.criterion = Criterion.firstHit
    val r2 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r2("predicted_$C-Drug") === "drugB")
    assert(r2("confidence") === 0.9)

    // criterion="weightedMax" scoring
    rule.criterion = Criterion.firstHit
    val r3 = model.predict(Map("BP" -> "HIGH", "K" -> 0.0621, "Age" -> 36, "Na" -> 0.5023))
    assert(r3("predicted_$C-Drug") === "drugB")
    assert(r3("confidence") === 0.9)
  }

  test("CompoundRule") {
    val pmml = """<PMML xmlns="http://www.dmg.org/PMML-4_4" version="4.4">
                 |        <Header description="Test Rule Model"/>
                 |        <DataDictionary numberOfFields="3">
                 |            <DataField name="predictedValue" optype="continuous" dataType="double"/>
                 |            <DataField name="X" optype="categorical" dataType="string">
                 |                <Value value="A"/>
                 |                <Value value="B"/>
                 |            </DataField>
                 |        </DataDictionary>
                 |        <RuleSetModel functionName="regression" algorithmName="RuleSet">
                 |            <MiningSchema>
                 |                <MiningField name="X" usageType="active"/>
                 |                <MiningField name="predictedValue" usageType="target"/>
                 |            </MiningSchema>
                 |            <Output>
                 |                <OutputField name="predictedValue" optype="continuous" dataType="double" feature="predictedValue"/>
                 |            </Output>
                 |            <RuleSet>
                 |                <RuleSelectionMethod criterion="firstHit"/>
                 |                {rules}
                 |            </RuleSet>
                 |        </RuleSetModel>
                 |    </PMML>""".stripMargin

    val simpleRule =
      """
        |<SimpleRule score="10.0" confidence="1" weight="1">
        |    <SimplePredicate field="X" operator="equal" value="A" />
        |</SimpleRule>
        |<SimpleRule score="20.0" confidence="1" weight="1">
        |    <True/>
        |</SimpleRule>
        |""".stripMargin

    val pmmlSimpleRule = pmml.replace("{rules}", simpleRule)
    val modelSimpleRule = Model.fromString(pmmlSimpleRule)
    val r11 = modelSimpleRule.predict(Array("A"))
    assert(r11(0) === 10.0)
    val r12 = modelSimpleRule.predict(Array("B"))
    assert(r12(0) === 20.0)

    val compoundRule =
      """
        |<CompoundRule>
        |    <True/>
        |    <SimpleRule score="10.0" confidence="1" weight="1">
        |        <SimplePredicate field="X" operator="equal" value="A" />
        |    </SimpleRule>
        |</CompoundRule>
        |<SimpleRule score="20.0" confidence="1" weight="1">
        |    <True/>
        |</SimpleRule>
        |""".stripMargin

    val pmmlCompoundRule = pmml.replace("{rules}", compoundRule)
    val modelCompoundRule = Model.fromString(pmmlCompoundRule)
    val r21 = modelCompoundRule.predict(Array("A"))
    assert(r21(0) === 10.0)
    val r22 = modelCompoundRule.predict(Array("B"))
    assert(r22(0) === 20.0)

    val outCompoundRule =
      """
        |<CompoundRule>
        |    <SimplePredicate field="X" operator="equal" value="A" />
        |    <SimpleRule score="10.0" confidence="1" weight="1">
        |        <True/>
        |    </SimpleRule>
        |</CompoundRule>
        |<SimpleRule score="20.0" confidence="1" weight="1">
        |    <True/>
        |</SimpleRule>
        |""".stripMargin

    val pmmlOutCompoundRule = pmml.replace("{rules}", outCompoundRule)
    val modelOutCompoundRule = Model.fromString(pmmlOutCompoundRule)
    val r31 = modelOutCompoundRule.predict(Array("A"))
    assert(r31(0) === 10.0)
    val r32 = modelOutCompoundRule.predict(Array("B"))
    assert(r32(0) === 20.0)
  }
}

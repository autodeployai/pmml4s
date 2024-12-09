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
    assert(model.classes.map(_.toVal) === Array("Iris-setosa", "Iris-versicolor", "Iris-virginica"))
    assert(model.field("continent").validValues.map(_.toVal) === Array("africa", "asia"))
    val r = model.predict(Array(2.0, 1.75, 10, "africa"))
    assert(r(0) === "Iris-setosa")
    assert(r(1) === 1.0)
  }

  test("an ensemble of regression trees whose results are combined by weighted averaging") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_weighted_average_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "day", "continent", "sepal_width"))
    assert(model.targetName === "sepal_length")
    val r = model.predict(Array(2.0, 1.75, 10, "africa", 1.0))
    assert(r(0) === 5.254244999999999)
  }

  test("the first for which the predicate element of a segment is satisfied") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_select_first_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "day", "continent"))
    assert(model.targetName === "Class")
    val r = model.predict(Array(2.0, 1.75, 10, "africa"))
    assert(r(0) === "Iris-setosa")
    assert(r(1) === 1.0)
  }

  test("chain of models which output a \"Pollen Index\"") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_model_chain_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("Class", "PollenIndex"))
    val r = model.predict(Array(2.0, 1.75, 30, 2.0))
    assert(r(0) === 1.5000000000000002)
  }

  test("the model composition approach where a decision tree model is used to select a regression model") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_model_composition_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("Class", "PollenIndex"))
    val r = model.predict(Array(2.0, 1.75, 30, 2.0))
    assert(r(0) === 0.7)
  }

  test("a more efficient implementation of the model composition approach where a decision tree model is used to select a regression model") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_mining_efficient_model_chain.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("petal_length", "petal_width", "temperature", "cloudiness"))
    assert(model.targetNames === Array("PollenIndex"))
    val r = model.predict(Array(2.0, 1.75, 30, 2.0))
    assert(r(0) === 0.7)
  }

  test("an ensemble of three trees in which each tree is weighted based on the node assignment") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_weighted_majority_vote_mining.xml")
    assert(model.modelElement === ModelElement.MiningModel)
    assert(model.inputNames === Array("SEPALLEN", "SEPALWID", "PETALLEN", "PETALWID"))
    assert(model.targetNames === Array("SPECIES"))
    val r = model.predict(Array(2.0, 1.75, 30, 2.0))
    assert(r(2) === 0.5555556666666667)
    assert(r(3) === 3)
  }

  test("a modelChain model with Output") {
    val model = Model.fromString(
      """
        |<PMML xmlns="http://www.dmg.org/PMML-4_4" version="4.4">
        |    <Header description="Test Rule Model"/>
        |    <DataDictionary numberOfFields="4">
        |        <DataField name="X" optype="categorical" dataType="string">
        |            <Value value="A"/>
        |            <Value value="B"/>
        |        </DataField>
        |        <DataField name="_predictedValue1" optype="continuous" dataType="double"/>
        |        <DataField name="_predictedValue2" optype="continuous" dataType="double"/>
        |        <DataField name="_predictedValueSum" optype="continuous" dataType="double"/>
        |        <DataField name="_predictedValueFinal" optype="continuous" dataType="double"/>
        |        <DataField name="predictedValueFinal" optype="continuous" dataType="double"/>
        |    </DataDictionary>
        |    <MiningModel functionName="regression">
        |        <MiningSchema>
        |            <MiningField name="X" usageType="active"/>
        |            <MiningField name="predictedValueFinal" usageType="target"/>
        |        </MiningSchema>
        |        <Segmentation multipleModelMethod="modelChain">
        |            <Segment id="0">
        |                <True/>
        |                <RuleSetModel functionName="regression" algorithmName="RuleSet">
        |                    <MiningSchema>
        |                        <MiningField name="X" usageType="active"/>
        |                        <MiningField name="_predictedValue1" usageType="target"/>
        |                    </MiningSchema>
        |                    <Output>
        |                        <OutputField name="_predictedValue1" optype="continuous" dataType="double" feature="predictedValue"/>
        |                    </Output>
        |                    <RuleSet>
        |                        <RuleSelectionMethod criterion="firstHit"/>
        |                        <SimpleRule score="100" confidence="1" weight="1">
        |                            <SimplePredicate field="X" operator="equal" value="A" />
        |                        </SimpleRule>
        |                        <SimpleRule score="-100" confidence="1" weight="1">
        |                            <True/>
        |                        </SimpleRule>
        |                    </RuleSet>
        |                </RuleSetModel>
        |            </Segment>
        |            <Segment id="1">
        |                <True/>
        |                <RuleSetModel functionName="regression" algorithmName="RuleSet">
        |                    <MiningSchema>
        |                        <MiningField name="X" usageType="active"/>
        |                        <MiningField name="_predictedValue1" usageType="active"/>
        |                        <MiningField name="_predictedValue2" usageType="active"/>
        |                    </MiningSchema>
        |                    <Output>
        |                        <OutputField name="_predictedValue2" optype="continuous" dataType="double" feature="predictedValue"/>
        |                        <OutputField name="_predictedValueSum" optype="continuous" dataType="double" feature="transformedValue">
        |                            <Apply function="+">
        |                                <FieldRef field="_predictedValue2"/>
        |                                <FieldRef field="_predictedValue1"/>
        |                            </Apply>
        |                        </OutputField>
        |                    </Output>
        |                    <RuleSet>
        |                        <RuleSelectionMethod criterion="firstHit"/>
        |                        <SimpleRule score="10" confidence="1" weight="1">
        |                            <SimplePredicate field="X" operator="equal" value="A" />
        |                        </SimpleRule>
        |                        <SimpleRule score="-10" confidence="1" weight="1">
        |                            <True/>
        |                        </SimpleRule>
        |                    </RuleSet>
        |                </RuleSetModel>
        |            </Segment>
        |        </Segmentation>
        |        <Output>
        |            <OutputField name="predictedValueFinal" optype="continuous" dataType="double" feature="transformedValue">
        |                <FieldRef field="_predictedValueSum"/>
        |            </OutputField>
        |        </Output>
        |    </MiningModel>
        |</PMML>
        |""".stripMargin)
    assert(model.predict(Map("X" -> "A"))("predictedValueFinal") === 110.0)
    assert(model.predict(Map("X" -> "B"))("predictedValueFinal") === -110.0)
  }
}

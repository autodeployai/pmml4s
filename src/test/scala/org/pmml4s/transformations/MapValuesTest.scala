/*
 * Copyright (c) 2024 AutoDeployAI
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
package org.pmml4s.transformations

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest._
import funsuite._
import org.pmml4s.common.{InlineTable, Row}
import org.pmml4s.data.{Series, StringVal}
import org.pmml4s.metadata.DataField
import org.pmml4s.model.Model

/**
 * Test cases of MapValues
 */
class MapValuesTest extends AnyFunSuite {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.000001)

  test("a simple example from DMG") {
    val field = new DataField("gender")
    field.index = 0
    val mapValues = new MapValues(Array(new FieldColumnPair(field, "shortForm")),
      new InlineTable(Array(
        new Row(Map("shortForm" -> StringVal("m"), "longForm" -> StringVal("male"))),
        new Row(Map("shortForm" -> StringVal("f"), "longForm" -> StringVal("female"))))),
      "longForm",
      Option(StringVal("Missing")),
      Option(StringVal("Not missing")),
      None
    )

    assert(mapValues.eval(Series("m")) === StringVal("male"))
    assert(mapValues.eval(Series("f")) === StringVal("female"))
    assert(mapValues.eval(Series(null)) === StringVal("Missing"))
    assert(mapValues.eval(Series("x")) === StringVal("Not missing"))
  }

  test("mapMissingTo and defaultValue both are given") {
    val pmml = """<?xml version="1.0" encoding="UTF-8"?>
                 |<PMML xmlns="http://www.dmg.org/PMML-4_4" version="4.3">
                 |    <Header description="Test Lookup Rule Model"/>
                 |    <DataDictionary>
                 |        <DataField name="input" optype="categorical" dataType="string"/>
                 |        <DataField name="output" optype="categorical" dataType="string"/>
                 |    </DataDictionary>
                 |    <TransformationDictionary>
                 |        <DefineFunction name="test_lookup" optype="categorical" dataType="string">
                 |            <ParameterField name="key" optype="categorical" dataType="string"/>
                 |            <MapValues defaultValue="DEFAULT" mapMissingTo="MISSING" outputColumn="value">
                 |                <FieldColumnPair field="key" column="key"/>
                 |                <InlineTable>
                 |                    <row>
                 |                        <key>A</key>
                 |                        <value>A_value</value>
                 |                    </row>
                 |                    <row>
                 |                        <key>B</key>
                 |                        <value>B_value</value>
                 |                    </row>
                 |                </InlineTable>
                 |            </MapValues>
                 |        </DefineFunction>
                 |    </TransformationDictionary>
                 |    <TreeModel functionName="regression" missingValuePenalty="1.0">
                 |        <MiningSchema>
                 |            <MiningField name="input" usageType="active"/>
                 |        </MiningSchema>
                 |        <Output>
                 |            <OutputField name="output" optype="categorical" dataType="string" feature="transformedValue">
                 |                <Apply function="test_lookup">
                 |                    <FieldRef field="input"/>
                 |                </Apply>
                 |            </OutputField>
                 |        </Output>
                 |        <Node score="0">
                 |            <True/>
                 |        </Node>
                 |    </TreeModel>
                 |</PMML>""".stripMargin
    val model = Model.fromString(pmml)

    assert(model.predict(Array("A"))(0)  === "A_value")
    assert(model.predict(Array("B"))(0)  === "B_value")
    assert(model.predict(Array("C"))(0)  === "DEFAULT")
    assert(model.predict(Array(null))(0)  === "MISSING")
  }

  test("mapMissingTo is given and defaultValue is not") {
    val pmml = """<?xml version="1.0" encoding="UTF-8"?>
             |<PMML xmlns="http://www.dmg.org/PMML-4_4" version="4.3">
             |    <Header description="Test Lookup Rule Model"/>
             |    <DataDictionary>
             |        <DataField name="input" optype="categorical" dataType="string"/>
             |        <DataField name="output" optype="categorical" dataType="string"/>
             |    </DataDictionary>
             |    <TransformationDictionary>
             |        <DefineFunction name="test_lookup" optype="categorical" dataType="string">
             |            <ParameterField name="key" optype="categorical" dataType="string"/>
             |            <MapValues mapMissingTo="MISSING" outputColumn="value">
             |                <FieldColumnPair field="key" column="key"/>
             |                <InlineTable>
             |                    <row>
             |                        <key>A</key>
             |                        <value>A_value</value>
             |                    </row>
             |                    <row>
             |                        <key>B</key>
             |                        <value>B_value</value>
             |                    </row>
             |                </InlineTable>
             |            </MapValues>
             |        </DefineFunction>
             |    </TransformationDictionary>
             |    <TreeModel functionName="regression" missingValuePenalty="1.0">
             |        <MiningSchema>
             |            <MiningField name="input" usageType="active"/>
             |        </MiningSchema>
             |        <Output>
             |            <OutputField name="output" optype="categorical" dataType="string" feature="transformedValue">
             |                <Apply function="test_lookup">
             |                    <FieldRef field="input"/>
             |                </Apply>
             |            </OutputField>
             |        </Output>
             |        <Node score="0">
             |            <True/>
             |        </Node>
             |    </TreeModel>
             |</PMML>""".stripMargin
    val model = Model.fromString(pmml)

    assert(model.predict(Array("A"))(0)  === "A_value")
    assert(model.predict(Array("B"))(0)  === "B_value")
    assert(model.predict(Array("C"))(0)  === null)
    assert(model.predict(Array(null))(0)  === "MISSING")
  }
}
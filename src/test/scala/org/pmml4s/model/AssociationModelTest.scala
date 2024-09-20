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
 * Association model cases come from DMG examples: http://dmg.org/pmml/v4-3/AssociationRules.html
 */
class AssociationModelTest extends BaseModelTest {

  test("recommendation") {
    val model = Model.fromFile("src/test/resources/models/association/dmg_association_recommendation.xml")
    assert(model.modelElement === ModelElement.AssociationModel)
    val association = model.asInstanceOf[AssociationModel]
    assert(association.inputNames === Array("transaction", "item"))
    assert(association.groupField.name === "transaction")
    assert(association.activeField.name === "item")

    // #1 - Cracker, Coke
    {
      val r_1 = model.predict(Array("1", "Cracker"))
      val r = model.predict(Array("1", "Coke"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Cracker -> Coke")
      assert(r(4) === "Coke")
      assert(r(5) === "3")
    }

    // #2 - Cracker, Water
    {
      val r_1 = model.predict(Array("2", "Cracker"))
      val r = model.predict(Array("2", "Water"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
      assert(r(6) === "Cracker -> Coke")
      assert(r(7) === "Coke")
      assert(r(8) === "3")
    }

    // #3 - Water, Coke
    {
      val r_1 = model.predict(Array("3", "Water"))
      val r = model.predict(Array("3", "Coke"))
      assert(r(0) === "Water -> Cracker")
      assert(r(1) === "Cracker")
      assert(r(2) === "2")
      assert(r(3) === "Water -> {Pear , Banana}")
      assert(r(4) === "{Pear , Banana}")
      assert(r(5) === "5")
    }

    // #4 - Cracker, Water, Coke
    {
      val r_1 = model.predict(Array("4", "Cracker"))
      val r_2 = model.predict(Array("4", "Water"))
      val r = model.predict(Array("4", "Coke"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
      assert(r(6) === "Cracker -> Coke")
      assert(r(7) === "Coke")
      assert(r(8) === "3")
    }

    // #5 - Cracker, Water, Banana, Apple
    {
      val r_1 = model.predict(Array("5", "Cracker"))
      val r_2 = model.predict(Array("5", "Water"))
      val r_3 = model.predict(Array("5", "Banana"))
      val r = model.predict(Array("5", "Apple"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
      assert(r(6) === "Cracker -> Coke")
      assert(r(7) === "Coke")
      assert(r(8) === "3")
    }
  }

  test("exclusiveRecommendation") {
    val model = Model.fromFile("src/test/resources/models/association/dmg_association_exclusive_recommendation.xml")
    assert(model.modelElement === ModelElement.AssociationModel)
    val association = model.asInstanceOf[AssociationModel]
    assert(association.inputNames === Array("transaction", "item"))
    assert(association.groupField.name === "transaction")
    assert(association.activeField.name === "item")

    // #1 - Cracker, Coke
    {
      val r_1 = model.predict(Array("1", "Cracker"))
      val r = model.predict(Array("1", "Coke"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
    }

    // #2 - Cracker, Water
    {
      val r_1 = model.predict(Array("2", "Cracker"))
      val r = model.predict(Array("2", "Water"))
      assert(r(0) === "Cracker -> Coke")
      assert(r(1) === "Coke")
      assert(r(2) === "3")
      assert(r(3) === "{Cracker , Water} -> Nachos")
      assert(r(4) === "Nachos")
      assert(r(5) === "4")
      assert(r(6) === "Water -> {Pear , Banana}")
      assert(r(7) === "{Pear , Banana}")
      assert(r(8) === "5")
    }

    // #3 - Water, Coke
    {
      val r_1 = model.predict(Array("3", "Water"))
      val r = model.predict(Array("3", "Coke"))
      assert(r(0) === "Water -> Cracker")
      assert(r(1) === "Cracker")
      assert(r(2) === "2")
      assert(r(3) === "Water -> {Pear , Banana}")
      assert(r(4) === "{Pear , Banana}")
      assert(r(5) === "5")
    }

    // #4 - Cracker, Water, Coke
    {
      val r_1 = model.predict(Array("4", "Cracker"))
      val r_2 = model.predict(Array("4", "Water"))
      val r = model.predict(Array("4", "Coke"))
      assert(r(0) === "{Cracker , Water} -> Nachos")
      assert(r(1) === "Nachos")
      assert(r(2) === "4")
      assert(r(3) === "Water -> {Pear , Banana}")
      assert(r(4) === "{Pear , Banana}")
      assert(r(5) === "5")
    }

    // #5 - Cracker, Water, Banana, Apple
    {
      val r_1 = model.predict(Array("5", "Cracker"))
      val r_2 = model.predict(Array("5", "Water"))
      val r_3 = model.predict(Array("5", "Banana"))
      val r = model.predict(Array("5", "Apple"))
      assert(r(0) === "Cracker -> Coke")
      assert(r(1) === "Coke")
      assert(r(2) === "3")
      assert(r(3) === "{Cracker , Water} -> Nachos")
      assert(r(4) === "Nachos")
      assert(r(5) === "4")
      assert(r(6) === "Water -> {Pear , Banana}")
      assert(r(7) === "{Pear , Banana}")
      assert(r(8) === "5")
    }
  }

  test("ruleAssociation") {
    val model = Model.fromFile("src/test/resources/models/association/dmg_association_rule_association.xml")
    assert(model.modelElement === ModelElement.AssociationModel)
    val association = model.asInstanceOf[AssociationModel]
    assert(association.inputNames === Array("transaction", "item"))
    assert(association.groupField.name === "transaction")
    assert(association.activeField.name === "item")

    // #1 - Cracker, Coke
    {
      val r_1 = model.predict(Array("1", "Cracker"))
      val r = model.predict(Array("1", "Coke"))
      assert(r(0) === "Cracker -> Coke")
      assert(r(1) === "Coke")
      assert(r(2) === "3")
    }

    // #2 - Cracker, Water
    {
      val r_1 = model.predict(Array("2", "Cracker"))
      val r = model.predict(Array("2", "Water"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
    }

    // #3 - Water, Coke
    {
      val r_1 = model.predict(Array("3", "Water"))
      val r = model.predict(Array("3", "Coke"))
      assert(r(0) === null)
    }

    // #4 - Cracker, Water, Coke
    {
      val r_1 = model.predict(Array("4", "Cracker"))
      val r_2 = model.predict(Array("4", "Water"))
      val r = model.predict(Array("4", "Coke"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
      assert(r(6) === "Cracker -> Coke")
      assert(r(7) === "Coke")
      assert(r(8) === "3")
    }

    // #5 - Cracker, Water, Banana, Apple
    {
      val r_1 = model.predict(Array("5", "Cracker"))
      val r_2 = model.predict(Array("5", "Water"))
      val r_3 = model.predict(Array("5", "Banana"))
      val r = model.predict(Array("5", "Apple"))
      assert(r(0) === "Cracker -> Water")
      assert(r(1) === "Water")
      assert(r(2) === "1")
      assert(r(3) === "Water -> Cracker")
      assert(r(4) === "Cracker")
      assert(r(5) === "2")
      assert(r(6) === null)
    }
  }

}

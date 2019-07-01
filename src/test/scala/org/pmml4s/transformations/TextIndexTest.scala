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
package org.pmml4s.transformations

import org.pmml4s.data.Series
import org.pmml4s.metadata.DataField
import org.pmml4s.xml.XmlImplicits._
import org.pmml4s.xml.{ElemTags, TransformationsBuilder}
import org.scalatest.FunSuite

import scala.io.Source
import scala.xml.pull.{EvElemStart, XMLEventReader}

/**
 * Test cases come from DMG: http://dmg.org/pmml/v4-3/Transformations.html#xsdElement_TextIndex
 */
class TextIndexTest extends FunSuite with TransformationsBuilder {

  test("wordSeparatorCharacterRE=[\\s\\-]") {
    val textIndex = new TextIndex(new DataField("textField"), new Constant("user friendly"), wordSeparatorCharacterRE = "[\\s\\-]")
    assert(textIndex.eval(Series.fromMap(Map("textField" -> "user-friendly"))) === 1)
    assert(textIndex.eval(Series.fromMap(Map("textField" -> "user friendly"))) === 1)
  }

  test("maxLevenshteinDistance") {
    // maxLevenshteinDistance = 1
    val textIndex = new TextIndex(new DataField("textField"), new Constant("brown fox"), maxLevenshteinDistance = 1, wordSeparatorCharacterRE = "[\\s]")
    assert(textIndex.eval(Series.fromMap(Map("textField" -> "The quick browny foxy jumps over the lazy dog. The brown fox runs away and to be with another brown foxy."))) === 2)

    // maxLevenshteinDistance = 2
    val textIndex2 = new TextIndex(new DataField("textField"), new Constant("brown fox"), maxLevenshteinDistance = 2, wordSeparatorCharacterRE = "[\\s]")
    assert(textIndex2.eval(Series.fromMap(Map("textField" -> "The quick browny foxy jumps over the lazy dog. The brown fox runs away and to be with another brown foxy."))) === 3)
  }

  test("countHits") {
    val text = "I have a doog. My dog is white. The doog is friendly"

    // countHits = CountHits.allHits
    val textIndex = new TextIndex(new DataField("textField"), new Constant("dog"), maxLevenshteinDistance = 1, wordSeparatorCharacterRE = "[\\s\\.]", countHits = CountHits.allHits)
    assert(textIndex.eval(Series.fromMap(Map("textField" -> text))) === 3)

    // countHits = CountHits.bestHits
    val textIndex2 = new TextIndex(new DataField("textField"), new Constant("dog"), maxLevenshteinDistance = 1, wordSeparatorCharacterRE = "[\\s\\.]", countHits = CountHits.bestHits)
    assert(textIndex2.eval(Series.fromMap(Map("textField" -> text))) === 1)
  }

  test("An example with maxLevenshteinDistance and case insensitive") {
    val text = "The Sun was setting while the captain's son reached the bounty island, minutes after their ship had sunk to the bottom of the ocean"

    // maxLevenshteinDistance = 1
    val derivedField = new DerivedField("sunFrequency", new TextIndex(new DataField("myTextField"), new Constant("sun"), isCaseSensitive = false, maxLevenshteinDistance = 1))
    val value = derivedField.eval(Series.fromMap(Map("myTextField" -> text)))
    assert(value === 3)

    // maxLevenshteinDistance = 0
    val derivedField2 = new DerivedField("sunFrequency", new TextIndex(new DataField("myTextField"), new Constant("sun"), isCaseSensitive = false, maxLevenshteinDistance = 0))
    val value2 = derivedField2.eval(Series.fromMap(Map("myTextField" -> text)))
    assert(value2 === 1)
  }

  test("An example with normalization") {
    val str =
      """<DefineFunction name="myIndexFunction" optype="continuous">
        |    <ParameterField name="reviewText"/>
        |    <ParameterField name="term"/>
        |    <TextIndex textField="reviewText" localTermWeights="binary" isCaseSensitive="false">
        |
        |      <TextIndexNormalization inField="string" outField="stem" regexField="regex">
        |        <InlineTable>
        |          <row>
        |            <string>interfaces?</string>
        |            <stem>interface</stem>
        |            <regex>true</regex>
        |          </row>
        |          <row>
        |            <string>is|are|seem(ed|s?)|were</string>
        |            <stem>be</stem>
        |            <regex>true</regex>
        |          </row>
        |          <row>
        |            <string>user friendl(y|iness)</string>
        |            <stem>user_friendly</stem>
        |            <regex>true</regex>
        |          </row>
        |        </InlineTable>
        |      </TextIndexNormalization>
        |
        |      <TextIndexNormalization inField="re" outField="feature" regexField="regex">
        |        <InlineTable>
        |          <row>
        |            <re>interface be (user_friendly|well designed|excellent)</re>
        |            <feature>ui_good</feature>
        |            <regex>true</regex>
        |          </row>
        |        </InlineTable>
        |      </TextIndexNormalization>
        |
        |      <FieldRef field="term"/>
        |
        |    </TextIndex>
        |  </DefineFunction>""".stripMargin

    val reader = new XMLEventReader(Source.fromString(str))
    val myIndexFunction = if (reader.hasNext) {
      reader.next match {
        case EvElemStart(_, ElemTags.DEFINE_FUNCTION, attrs, _) => {
          makeDefineFunction(reader, attrs)
        }
        case _                                                  => ???
      }
    } else ???

    val derivedField = new DerivedField("isGoodUI",
      new Apply(myIndexFunction, Seq(new FieldRef(new DataField("Review")), new Constant("ui_good"))))

    val text = "Testing the app for a few days convinced me the interfaces are excellent!"
    val value = derivedField.eval(Series.fromMap(Map("Review" -> text)))
    assert(value === 1)
  }
}
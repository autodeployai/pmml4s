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

import org.pmml4s.common.{StructField, _}
import org.pmml4s.data.Series
import org.pmml4s.metadata.{AttributeType, ContinuousAttribute, ResultFeature}

import scala.io.Source

/**
 * Model cases come from DMG examples: http://dmg.org/pmml/pmml_examples/index.html
 */
class ModelTest extends BaseModelTest {

  test("get model info") {
    val model = Model.fromFile("src/test/resources/models/tree/single_audit_dectree.xml")
    assert(model.version === "4.1")
    assert(model.dVersion === 4.1)
    assert(model.header.copyright === Some("KNIME"))
    assert(model.header.application.get.name === "KNIME")
    assert(model.header.application.get.version === Some("2.8.0"))

    val dataDictionary = model.dataDictionary
    assert(dataDictionary.size === 10)
    assert(dataDictionary.fieldNames.sameElements(Array("Age", "Employment", "Education", "Marital", "Occupation", "Income", "Gender", "Deductions", "Hours", "TARGET_Adjusted")))

    val inputFields = model.inputFields
    assert(inputFields.map(_.name).sameElements(Array("Age", "Employment", "Education", "Marital", "Occupation", "Income", "Gender", "Deductions", "Hours")))

    val f = dataDictionary("Age")
    assert(f.name === "Age")
    assert(f.dataType === DataType.integer)
    assert(f.opType === OpType.continuous)
    assert(f.attrType === AttributeType.Continuous)
    assert(f.attribute.asInstanceOf[ContinuousAttribute].intervals.head === Interval(17.0, 90.0, Closure.closedClosed))

    val f2 = dataDictionary("Employment")
    assert(f2.name === "Employment")
    assert(f2.dataType === DataType.string)
    assert(f2.opType === OpType.nominal)
    assert(f2.attrType === AttributeType.Categorical)
    assert(f2.validValues.sameElements(Array("Private", "Consultant", "SelfEmp", "PSLocal", "PSState", "PSFederal", "Unemployed", "NA", "Volunteer")))

    val targetField = model.targetField
    assert(targetField.name === "TARGET_Adjusted")
    assert(targetField.dataType === DataType.string)
    assert(targetField.opType === OpType.nominal)
    assert(targetField.attrType === AttributeType.Categorical)
    assert(targetField.validValues.sameElements(Array("0", "1")))

    val targetFields = model.targetFields
    assert(targetFields.map(_.name).sameElements(Array("TARGET_Adjusted")))

    val outputFields = model.candidateOutputFields
    assert(outputFields.map(_.name).sameElements(Array("predicted_TARGET_Adjusted", "probability", "probability_0", "probability_1", "node_id")))
    assert(outputFields.map(_.feature).sameElements(Array(ResultFeature.predictedValue, ResultFeature.probability, ResultFeature.probability, ResultFeature.probability, ResultFeature.entityId)))
    assert(outputFields(1).value.isEmpty)
    assert(outputFields(2).value === Some("0"))
    assert(outputFields(3).value === Some("1"))

    assert(model.inputSchema === StructType(Array(
      StructField("Age", DataType.integer),
      StructField("Employment", DataType.string),
      StructField("Education", DataType.string),
      StructField("Marital", DataType.string),
      StructField("Occupation", DataType.string),
      StructField("Income", DataType.double),
      StructField("Gender", DataType.string),
      StructField("Deductions", DataType.double),
      StructField("Hours", DataType.integer))))
    assert(model.outputSchema === StructType(Array(
      StructField("predicted_TARGET_Adjusted", DataType.string),
      StructField("probability", DataType.real),
      StructField("probability_0", DataType.real),
      StructField("probability_1", DataType.real),
      StructField("node_id", DataType.string)
    )))

    val tree = model.asInstanceOf[TreeModel]
    assert(tree.splitCharacteristic === SplitCharacteristic.multiSplit)
    assert(tree.missingValueStrategy === MissingValueStrategy.lastPrediction)
    assert(tree.noTrueChildStrategy === NoTrueChildStrategy.returnNullPrediction)
  }

  test("make prediction") {
    val model = Model.fromFile("src/test/resources/models/tree/single_iris_dectree.xml")
    assert(model.version === "4.1")
    assert(model.dVersion === 4.1)
    assert(model.header.copyright === Some("KNIME"))
    assert(model.header.application.get.name === "KNIME")
    assert(model.header.application.get.version === Some("2.8.0"))
    assert(model.dataDictionary.size === 5)

    val f = model.dataDictionary("sepal_length")
    assert(f.name === "sepal_length")
    assert(f.dataType === DataType.double)
    assert(f.opType === OpType.continuous)
    assert(f.attrType === AttributeType.Continuous)
    assert(f.attribute.asInstanceOf[ContinuousAttribute].intervals.head === Interval(4.3, 7.9, Closure.closedClosed))

    val t = model.dataDictionary("class")
    assert(t.name === "class")
    assert(t.dataType === DataType.string)
    assert(t.opType === OpType.nominal)
    assert(t.attrType === AttributeType.Categorical)
    assert(t.validValues.sameElements(Array("Iris-setosa", "Iris-versicolor", "Iris-virginica")))

    assert(model.modelElement === ModelElement.TreeModel)
    assert(model.modelName === Some("DecisionTree"))
    assert(model.functionName === MiningFunction.classification)
    assert(model.algorithmName === None)
    assert(model.isScorable)

    assert(model.inputNames.sameElements(Array("sepal_length", "sepal_width", "petal_length", "petal_width")))
    assert(model.inputFields.length === 4)
    assert(model.targetNames.sameElements(Array("class")))
    assert(model.targetName === "class")
    assert(model.targetFields.length === 1)
    assert(model.targetField.name === "class")
    assert(model.opType === OpType.nominal)
    assert(model.classes.sameElements(Array("Iris-setosa", "Iris-versicolor", "Iris-virginica")))
    assert(model.numClasses === 3)
    assert(model.inputSchema === StructType(Array(StructField("sepal_length", DataType.double),
      StructField("sepal_width", DataType.double),
      StructField("petal_length", DataType.double),
      StructField("petal_width", DataType.double))))
    assert(model.outputSchema.length === 6)
    assert(model.isClassification)

    val tree = model.asInstanceOf[TreeModel]
    assert(tree.splitCharacteristic === SplitCharacteristic.binarySplit)
    assert(tree.missingValueStrategy === MissingValueStrategy.lastPrediction)
    assert(tree.noTrueChildStrategy === NoTrueChildStrategy.returnNullPrediction)

    // Series with schema
    val schema = model.inputSchema
    val r = model.predict(Series.fromArray(Array(5.1, 3.5, 1.4, 0.2), schema))
    assert(r.schema === model.outputSchema)
    assert(r.toArray.sameElements(Array("Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1")))
    val r2 = model.predict(Series.fromArray(Array(7, 3.2, 4.7, 1.4), schema))
    assert(r2.schema === model.outputSchema)
    assert(r2.toArray.sameElements(Array("Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3")))

    // Series without schema
    val r3 = model.predict(Series.fromArray(Array(5.1, 3.5, 1.4, 0.2)))
    assert(r3.schema === model.outputSchema)
    assert(r3.toArray.sameElements(Array("Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1")))
    val r4 = model.predict(Series.fromArray(Array(7, 3.2, 4.7, 1.4)))
    assert(r4.schema === model.outputSchema)
    assert(r4.toArray.sameElements(Array("Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3")))

    // Array
    val r5 = model.predict(Array(5.1, 3.5, 1.4, 0.2))
    assert(r5.sameElements(Array("Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1")))
    val r6 = model.predict(Array(7, 3.2, 4.7, 1.4))
    assert(r6.sameElements(Array("Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3")))
  }

  test("custom model outputs") {
    val model = Model.fromFile("src/test/resources/models/mining/dmg_weighted_majority_vote_mining_revised.xml")
    assert(model.output.isDefined)
    assert(!model.supplementOutput)
    assert(model.customOutputFields.isEmpty)
    assert(model.outputNames === Array("PROB_1", "PROB_2"))
    assert(model.defaultOutputFields.map(_.name) === Array("predicted_SPECIES", "probability", "probability_1", "probability_3", "probability_2"))
    val r = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r.length === 2)

    // Set the flag `supplementOutput` to true
    model.setSupplementOutput(true)
    assert(model.supplementOutput)
    assert(model.outputNames === Array("PROB_1", "PROB_2", "predicted_SPECIES", "probability", "probability_3"))
    val r2 = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r2.length === 5)
    assert(r2(2) === 3)
    assert(r2(3) === 0.5555556666666667)

    // Specify custom outputs with only the prediction
    val outputFields = model.outputFields
    model.setOutputFields(Array(outputFields(2)))
    assert(model.outputNames === Array("predicted_SPECIES"))
    val r3 = model.predict(Series(2.0, 1.75, 30, 2.0))
    assert(r3.length === 1)
    assert(r3(0) === 3)
  }  
}

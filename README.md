# PMML4S

_PMML4S_ is a PMML (Predictive Model Markup Language) scoring library in Scala.

## Features
_PMML4S_ is a lightweight, clean and efficient implementation based on the PMML specification from 2.0 through to the latest 4.3.

It supports the following models:
* [Association Rules](http://dmg.org/pmml/v4-3/AssociationRules.html)
* [Cluster Models](http://dmg.org/pmml/v4-3/ClusteringModel.html)
* [General Regression](http://dmg.org/pmml/v4-3/GeneralRegression.html)
* [k-Nearest Neighbors](http://dmg.org/pmml/v4-3/KNN.html)
* [Naive Bayes](http://dmg.org/pmml/v4-3/NaiveBayes.html)
* [Neural Network](http://dmg.org/pmml/v4-3/NeuralNetwork.html)
* [Regression](http://dmg.org/pmml/v4-3/Regression.html)
* [Ruleset](http://dmg.org/pmml/v4-3/RuleSet.html)
* [Scorecard](http://dmg.org/pmml/v4-3/Scorecard.html)
* [Trees](http://dmg.org/pmml/v4-3/TreeModel.html)
* [Vector Machine](http://dmg.org/pmml/v4-3/SupportVectorMachine.html)
* [Multiple Models](http://dmg.org/pmml/v4-3/MultipleModels.html)

## Usage
_PMML4S_ is really easy to use. Just do one or more of the following:

1. Load model.

```scala
import org.pmml4s.model.Model
import scala.io.Source

// load a model from an IO source that supports various sources, e.g. from a URL locates a PMML model.
val model = Model(Source.fromURL(new java.net.URL("http://dmg.org/pmml/pmml_examples/KNIME_PMML_4.1_Examples/single_iris_dectree.xml")))
```

or

```scala
import org.pmml4s.model.Model

// load a model from those help methods, e.g. pathname, file object, a string, an array of bytes, or an input stream.
val model = Model.fromFile("single_iris_dectree.xml")
```

2. Call `predict(values)` to predict new values that can be in different types, and the type of results is always same as inputs.

* `values` in a Map:

```scala
scala> val result = model.predict(Map("sepal_length" -> 5.1, "sepal_width" -> 3.5, "petal_length" -> 1.4, "petal_width" -> 0.2))
result: Map[String,Any] = Map(Probability -> 1.0, Probability_Iris-versicolor -> 0.0, Probability_Iris-setosa -> 1.0, Probability_Iris-virginica -> 0.0, PredictedValue -> Iris-setosa, Node_ID -> 1)
```

* `values` in a list of pairs of keys and values:

```scala
scala> val result = model.predict("sepal_length" -> 5.1, "sepal_width" -> 3.5, "petal_length" -> 1.4, "petal_width" -> 0.2)
result: Seq[(String, Any)] = ArraySeq((PredictedValue,Iris-setosa), (Probability,1.0), (Probability_Iris-setosa,1.0), (Probability_Iris-versicolor,0.0), (Probability_Iris-virginica,0.0), (Node_ID,1))
```

* `values` in an Array, the order of those values is supposed as same as the input fields list, and the order of results is same as the output fields list.

```scala
scala> val inputNames = model.inputNames
inputNames: Array[String] = Array(sepal_length, sepal_width, petal_length, petal_width)

scala> val result = model.predict(Array(5.1, 3.5, 1.4, 0.2))
result: Array[Any] = Array(Iris-setosa, 1.0, 1.0, 0.0, 0.0, 1)

scala> val outputNames = model.outputNames
outputNames: Array[String] = Array(PredictedValue, Probability, Probability_Iris-setosa, Probability_Iris-versicolor, Probability_Iris-virginica, Node_ID)
```

* `values` in the JSON format that supports the following styles. The JSON string can take more than more records to predict, and the results are still a string in JSON with the same format as input.
    - `records` : list like [{column -> value}, … , {column -> value}]
    - `split` : dict like {‘columns’ -> [columns], ‘data’ -> [values]}

```scala
scala> val result = model.predict("""[{"sepal_length": 5.1, "sepal_width": 3.5, "petal_length": 1.4, "petal_width": 0.2}, {"sepal_length": 7, "sepal_width": 3.2, "petal_length": 4.7, "petal_width": 1.4}]""")
result: String = [{"Probability":1.0,"Probability_Iris-versicolor":0.0,"Probability_Iris-setosa":1.0,"Probability_Iris-virginica":0.0,"PredictedValue":"Iris-setosa","Node_ID":"1"},{"Probability":0.9074074074074074,"Probability_Iris-versicolor":0.9074074074074074,"Probability_Iris-setosa":0.0,"Probability_Iris-virginica":0.09259259259259259,"PredictedValue":"Iris-versicolor","Node_ID":"3"}]

scala> val result = model.predict("""{"columns": ["sepal_length", "sepal_width", "petal_length", "petal_width"], "data":[[5.1, 3.5, 1.4, 0.2], [7, 3.2, 4.7, 1.4]]}""")
result: String = {"columns":["PredictedValue","Probability","Probability_Iris-setosa","Probability_Iris-versicolor","Probability_Iris-virginica","Node_ID"],"data":[["Iris-setosa",1.0,1.0,0.0,0.0,"1"],["Iris-versicolor",0.9074074074074074,0.0,0.9074074074074074,0.09259259259259259,"3"]]}
```

* `values` in the _PMML4S's_ `Series`:

```scala
import org.pmml4s.data.Series
import org.pmml4s.util.Utils

// The input schema contains a list of input fields with its name and data type, you can prepare data based on it.
scala> val inputSchema = model.inputSchema
inputSchema: org.pmml4s.common.StructType = StructType(StructField(sepal_length,double), StructField(sepal_width,double), StructField(petal_length,double), StructField(petal_width,double))

// Suppose the row is a record in map from an external columnar data, e.g. a CSV file, or relational database.
val row = Map("sepal_length" -> "5.1", "sepal_width" -> "3.5", "petal_length" -> "1.4", "petal_width" -> "0.2")

// You need to convert the data to the desired type defined by PMML, and keep the same order as defined in the input schema.
val values = inputSchema.map(x => Utils.toVal(row(x.name), x.dataType))

scala> val result = model.predict(Series.fromSeq(values))
result: org.pmml4s.data.Series = [Iris-setosa,1.0,1.0,0.0,0.0,1],[(PredictedValue,string),(Probability,double),(Probability_Iris-setosa,double),(Probability_Iris-versicolor,double),(Probability_Iris-virginica,double),(Node_ID,string)]

// You can also create a Series with schema, so that values will be accessed by names, the order of values is trivial, e.g.
scala> val result = model.predict(Series.fromSeq(values.reverse, org.pmml4s.common.StructType(inputSchema.fields.reverse)))
result: org.pmml4s.data.Series = [Iris-setosa,1.0,1.0,0.0,0.0,1], [(PredictedValue,string),(Probability,double),(Probability_Iris-setosa,double),(Probability_Iris-versicolor,double),(Probability_Iris-virginica,double),(Node_ID,string)]
```

You can use any formats of values according to your environment. Except of the `Series` need to convert the data explicitly, you don't need to call `Utils.toVal` explicitly to convert data to ones defined by PMML for others, the conversion will be operated properly automatically. e.g. those input values are string, not double, you can still get the same correct results.

```scala
scala> val result = model.predict(Map("sepal_length" -> "5.1", "sepal_width" -> "3.5", "petal_length" -> "1.4", "petal_width" -> "0.2"))
result: Map[String,Any] = Map(Probability -> 1.0, Probability_Iris-versicolor -> 0.0, Probability_Iris-setosa -> 1.0, Probability_Iris-virginica -> 0.0, PredictedValue -> Iris-setosa, Node_ID -> 1)

scala> val result = model.predict(Array("5.1", "3.5", "1.4", "0.2"))
result: Array[Any] = Array(Iris-setosa, 1.0, 1.0, 0.0, 0.0, 1)   
```

3. Understand the scoring result.

You can see the names of output fields, like `PredictedValue`, `Probability`, actually those names are trivial, the PMML can use any names for those output fields. 

The most important attribute of `OutputField` is `feature` that specifies the value the output field takes from the computed mining result, about other attributes, see [DMG](http://dmg.org/pmml/v4-3/Output.html#xsdType_RESULT-FEATURE)

The `Output` is optional for PMML, if it's not present, _PMML4S_ will produce some predefined output fields, e.g.

  Name                      | Description
  ------------------------- | ----------------------
  PredictedValue            | Predicted value for the most models
  Probability               | Probability of the predicted categorical value for the classification model
  Probability_{category}    | Probability of the specified category for the classification model
  Confidence                | Confidence of the specified category for the classification model
  Node_ID                   | ID of the hit node for the tree model
  PredictedDisplayValue     | Could be the name of the winning cluster for the clustering model
  Distance                  | Distance to the predicted entity for the clustering model
  Similarity                | Similarity to the predicted entity for the clustering model
  ReasonCode_{rank}         | Reason code of rank for the scorecard model

```scala
// The output fields describe the result values.
val outputFields = model.outputFields

scala> outputFields.foreach(println)
OutputField(name=PredictedValue, displayName=Some(Predicted value of class), dataType=string, opType=nominal, feature=predictedValue, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
OutputField(name=Probability, displayName=Some(Probability of predicted value), dataType=double, opType=continuous, feature=probability, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
OutputField(name=Probability_Iris-setosa, displayName=Some(Probability of Iris-setosa), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-setosa), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
OutputField(name=Probability_Iris-versicolor, displayName=Some(Probability of Iris-versicolor), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-versicolor), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
OutputField(name=Probability_Iris-virginica, displayName=Some(Probability of Iris-virginica), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-virginica), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
OutputField(name=Node_ID, displayName=Some(ID of hit node), dataType=string, opType=nominal, feature=entityId, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
```

## Use in Spark
See the [PMML4S-Spark](https://github.com/autodeployai/pmml4s-spark) project.

## Use in PySpark
See the [PyPMML-Spark](https://github.com/autodeployai/pypmml-spark) project.

## Use in Python
See the [PyPMML](https://github.com/autodeployai/pypmml) project.

## Attention
Except of the Association model with transaction, all model objects are read-only, so you are free to use them in a multi-threaded environment.

## Support
If you have any questions about the _PMML4S_ library, please open issues on this repository.

Feedback and contributions to the project, no matter what kind, are always very welcome. 

## License
_PMML4S_ is licensed under [APL 2.0](http://www.apache.org/licenses/LICENSE-2.0).

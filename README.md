# PMML4S

_PMML4S_ is a PMML (Predictive Model Markup Language) scoring library for Scala. It provides both Scala and Java Evaluator API for PMML.

## Features
_PMML4S_ is a lightweight, clean and efficient implementation based on the PMML specification from 2.0 through to the latest 4.4.

### Model types support

It supports the following models:
* [Anomaly Detection Models](http://dmg.org/pmml/v4-4/AnomalyDetectionModel.html)
* [Association Rules](http://dmg.org/pmml/v4-4/AssociationRules.html)
* [Cluster Models](http://dmg.org/pmml/v4-4/ClusteringModel.html)
* [General Regression](http://dmg.org/pmml/v4-4/GeneralRegression.html)
* [k-Nearest Neighbors](http://dmg.org/pmml/v4-4/KNN.html)
* [Naive Bayes](http://dmg.org/pmml/v4-4/NaiveBayes.html)
* [Neural Network](http://dmg.org/pmml/v4-4/NeuralNetwork.html)
* [Regression](http://dmg.org/pmml/v4-4/Regression.html)
* [Ruleset](http://dmg.org/pmml/v4-4/RuleSet.html)
* [Scorecard](http://dmg.org/pmml/v4-4/Scorecard.html)
* [Trees](http://dmg.org/pmml/v4-4/TreeModel.html)
* [Vector Machine](http://dmg.org/pmml/v4-4/SupportVectorMachine.html)
* [Multiple Models](http://dmg.org/pmml/v4-4/MultipleModels.html)

Not yet supported models:
* [Baseline Models](http://dmg.org/pmml/v4-4/BaselineModel.html)
* [Bayesian Network](http://dmg.org/pmml/v4-4/BayesianNetwork.html)
* [Gaussian Process](http://dmg.org/pmml/v4-4/GaussianProcess.html)
* [Sequences](http://dmg.org/pmml/v4-4/Sequence.html)
* [Text Models](http://dmg.org/pmml/v4-4/Text.html)
* [Time Series](http://dmg.org/pmml/v4-4/TimeSeriesModel.html)

### Transformations support

It supports the following transformations:
* Normalization
* Discretization
* Value mapping
* Text Indexing
* Functions

Not yet supported transformations:
* Aggregation
* Lag

## Installation
_PMML4S_ is available from maven central.

Latest release: [![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.pmml4s/pmml4s_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.pmml4s/pmml4s_2.12)

##### SBT users
```scala
libraryDependencies += "org.pmml4s" %%  "pmml4s" % "0.9.2"
```

##### Maven users
```xml
<dependency>
  <groupId>org.pmml4s</groupId>
  <artifactId>pmml4s_${scala.version}</artifactId>
  <version>0.9.2</version>
</dependency>
```

## Use in Scala
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

    * **`values` in a Map:**

    ```scala
    scala> val result = model.predict(Map("sepal_length" -> 5.1, "sepal_width" -> 3.5, "petal_length" -> 1.4, "petal_width" -> 0.2))
    result: Map[String,Any] = Map(probability -> 1.0, probability_Iris-versicolor -> 0.0, probability_Iris-setosa -> 1.0, probability_Iris-virginica -> 0.0, predicted_class -> Iris-setosa, node_id -> 1)
    ```

    * **`values` in a list of pairs of keys and values:**

    ```scala
    scala> val result = model.predict("sepal_length" -> 5.1, "sepal_width" -> 3.5, "petal_length" -> 1.4, "petal_width" -> 0.2)
    result: Seq[(String, Any)] = ArraySeq((predicted_class,Iris-setosa), (probability,1.0), (probability_Iris-setosa,1.0), (probability_Iris-versicolor,0.0), (probability_Iris-virginica,0.0), (node_id,1))
    ```

    * **`values` in an Array:**

    The order of those values is supposed as same as the input fields list, and the order of results is same as the output fields list.

    ```scala
    scala> val inputNames = model.inputNames
    inputNames: Array[String] = Array(sepal_length, sepal_width, petal_length, petal_width)

    scala> val result = model.predict(Array(5.1, 3.5, 1.4, 0.2))
    result: Array[Any] = Array(Iris-setosa, 1.0, 1.0, 0.0, 0.0, 1)

    scala> val outputNames = model.outputNames
    outputNames: Array[String] = Array(predicted_class, probability, probability_Iris-setosa, probability_Iris-versicolor, probability_Iris-virginica, node_id)
    ```

    * **`values` in the JSON format:**

    It supports the following styles, and the JSON string can take more than more records to predict, and the results are still a string in JSON with the same format as input.
    
        - `records` : list like [{column -> value}, … , {column -> value}]
        - `split` : dict like {‘columns’ -> [columns], ‘data’ -> [values]}

    ```scala
    scala> val result = model.predict("""[{"sepal_length": 5.1, "sepal_width": 3.5, "petal_length": 1.4, "petal_width": 0.2}, {"sepal_length": 7, "sepal_width": 3.2, "petal_length": 4.7, "petal_width": 1.4}]""")
    result: String = [{"probability":1.0,"probability_Iris-versicolor":0.0,"probability_Iris-setosa":1.0,"probability_Iris-virginica":0.0,"predicted_class":"Iris-setosa","node_id":"1"},{"probability":0.9074074074074074,"probability_Iris-versicolor":0.9074074074074074,"probability_Iris-setosa":0.0,"probability_Iris-virginica":0.09259259259259259,"predicted_class":"Iris-versicolor","node_id":"3"}]

    scala> val result = model.predict("""{"columns": ["sepal_length", "sepal_width", "petal_length", "petal_width"], "data":[[5.1, 3.5, 1.4, 0.2], [7, 3.2, 4.7, 1.4]]}""")
    result: String = {"columns":["predicted_class","probability","probability_Iris-setosa","probability_Iris-versicolor","probability_Iris-virginica","node_id"],"data":[["Iris-setosa",1.0,1.0,0.0,0.0,"1"],["Iris-versicolor",0.9074074074074074,0.0,0.9074074074074074,0.09259259259259259,"3"]]}
    ```

    * **`values` in the _PMML4S's_ `Series`:**

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
    result: org.pmml4s.data.Series = [Iris-setosa,1.0,1.0,0.0,0.0,1],[(predicted_class,string),(probability,double),(probability_Iris-setosa,double),(probability_Iris-versicolor,double),(probability_Iris-virginica,double),(node_id,string)]

    // You can also create a Series with schema, so that values will be accessed by names, the order of values is trivial, e.g.
    scala> val result = model.predict(Series.fromSeq(values.reverse, org.pmml4s.common.StructType(inputSchema.fields.reverse)))
    result: org.pmml4s.data.Series = [Iris-setosa,1.0,1.0,0.0,0.0,1], [(predicted_class,string),(probability,double),(probability_Iris-setosa,double),(probability_Iris-versicolor,double),(probability_Iris-virginica,double),(node_id,string)]
    ```

    **Which format to use?**

    You can use any formats of values according to your environment. Except of the `Series` that need to convert the data explicitly, you don't need to call `Utils.toVal` explicitly to convert data to ones defined by PMML for others, the conversion will be operated properly automatically. e.g. those input values are string, not double, you can still get the same correct results.

    ```scala
    scala> val result = model.predict(Map("sepal_length" -> "5.1", "sepal_width" -> "3.5", "petal_length" -> "1.4", "petal_width" -> "0.2"))
    result: Map[String,Any] = Map(probability -> 1.0, probability_Iris-versicolor -> 0.0, probability_Iris-setosa -> 1.0, probability_Iris-virginica -> 0.0, predicted_class -> Iris-setosa, node_id -> 1)

    scala> val result = model.predict(Array("5.1", "3.5", "1.4", "0.2"))
    result: Array[Any] = Array(Iris-setosa, 1.0, 1.0, 0.0, 0.0, 1)   
    ```

3. Understand the result values.

    You can see the names of output fields, like `predicted_class`, `probability`, actually those names are trivial, the PMML can use any names for those output fields. 

    The most important attribute of `OutputField` is `feature` that specifies the value the output field takes from the computed mining result, about other attributes, see [DMG](http://dmg.org/pmml/v4-3/Output.html#xsdType_RESULT-FEATURE)

    The `Output` is optional for PMML, if it's not present, _PMML4S_ will produce some predefined output fields, e.g.

    Name                      | Description
    ------------------------- | ----------------------
    predicted_{target}        | Predicted value of the specified target for the supervised model
    probability               | Probability of the predicted categorical value for the classification model
    probability_{category}    | Probability of the specified category for the classification model
    confidence                | Confidence of the specified category for the classification model
    node_id                   | ID of the hit node for the tree model
    cluster                   | Identifier of the winning cluster for the clustering model
    cluster_name              | Name of the winning cluster for the clustering model
    distance                  | Distance to the predicted entity for the clustering model
    similarity                | Similarity to the predicted entity for the clustering model
    reason_code_{rank}        | Reason code of rank for the scorecard model
    anomalyScore              | Anomaly score of the anomaly detection model

    Call `outputFields()` to get a list of output fields describe result values.

    ```scala
    // The output fields describe the result values.
    val outputFields = model.outputFields

    scala> outputFields.foreach(println)
    OutputField(name=predicted_class, displayName=Some(Predicted value of class), dataType=string, opType=nominal, feature=predictedValue, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    OutputField(name=probability, displayName=Some(probability of predicted value), dataType=double, opType=continuous, feature=probability, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    OutputField(name=probability_Iris-setosa, displayName=Some(probability of Iris-setosa), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-setosa), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    OutputField(name=probability_Iris-versicolor, displayName=Some(probability of Iris-versicolor), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-versicolor), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    OutputField(name=probability_Iris-virginica, displayName=Some(probability of Iris-virginica), dataType=double, opType=continuous, feature=probability, targetField=None, value=Some(Iris-virginica), ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    OutputField(name=node_id, displayName=Some(ID of hit node), dataType=string, opType=nominal, feature=entityId, targetField=None, value=None, ruleFeature=consequent, algorithm=exclusiveRecommendation, rank=1, rankBasis=confidence, rankOrder=descending, isMultiValued=false, segmentId=None, isFinalResult=true, decisions=None, expr=None)
    ```

## Use in Java
It's also easy to use and similar as Scala.

1. Load model.

    ```java
    import org.pmml4s.model.Model;

    Model model = Model.fromFile("single_iris_dectree.xml");
    ```

2. Call `predict(values)` to predict new values that can be in different types, and the type of results is always same as inputs.

    * **`values` in a Map of Java:**

    ```java
    import java.util.Map;
    import java.util.HashMap;

    Map result = model.predict(new HashMap<String, Object>() {{
                put("sepal_length", 5.1);
                put("sepal_width", 3.5);
                put("petal_length", 1.4);
                put("petal_width", 0.2);
            }});
    ```

    * **`values` in an Array:**

    The order of those values is supposed as same as the input fields list, and the order of results is same as the output fields list.

    ```java
    String[] inputNames = model.inputNames();
    Object[] result = model.predict(new Double[]{5.1, 3.5, 1.4, 0.2});
    ```

    * **`values` in the JSON format:**

    It's same as Scala

    * **`values` in the _PMML4S's_ `Series`:**

    ```java
    import org.pmml4s.data.Series;
    import org.pmml4s.util.Utils;
    import org.pmml4s.common.StructType;
    import org.pmml4s.common.StructField;

    // The input schema contains a list of input fields with its name and data type, you can prepare data based on it.
    StructType inputSchema = model.inputSchema();

    // Suppose the row is a record in map from an external columnar data, e.g. a CSV file, or relational database.
    Map row = new HashMap<String, String>() {{
                put("sepal_length", "5.1");
                put("sepal_width", "3.5");
                put("petal_length", "1.4");
                put("petal_width", "0.2");
            }}

    // You need to convert the data to the desired type defined by PMML, and keep the same order as defined in the input schema.
    Object[] values = new Object[inputSchema.size()];
    for (int i = 0; i < values.length; i++) {
      StructField sf = inputSchema.apply(i);
      values[i] = Utils.toVal(row.get(sf.name()), sf.dataType());
    }

    Series result = model.predict(Series.fromArray(values))

    // You can also create a Series with schema, so that values will be accessed by names, the order of values is trivial, e.g.
    Series result = model.predict(Series.fromArray(values, inputSchema)))
    ```

3. Understand the result values. See details in Scala above 

## Use in Spark
See the [PMML4S-Spark](https://github.com/autodeployai/pmml4s-spark) project. _PMML4S-Spark_ is a PMML scoring library for Spark as SparkML Transformer.

## Use in PySpark
See the [PyPMML-Spark](https://github.com/autodeployai/pypmml-spark) project. _PyPMML-Spark_ is a Python PMML scoring library for PySpark as SparkML Transformer, it really is the Python API for PMML4s-Spark.

## Use in Python
See the [PyPMML](https://github.com/autodeployai/pypmml) project. _PyPMML_ is a Python PMML scoring library, it really is the Python API for PMML4S.

## Deploy PMML as REST API
See the [DaaS](https://www.autodeploy.ai/) system that deploys AI & ML models in production at scale on Kubernetes.

## Attention
Except of the Association model with transaction, all model objects are read-only, so you are free to use them in a multi-threaded environment.

## Support
If you have any questions about the _PMML4S_ library, please open issues on this repository.

Feedback and contributions to the project, no matter what kind, are always very welcome. 

## License
_PMML4S_ is licensed under [APL 2.0](http://www.apache.org/licenses/LICENSE-2.0).

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
package org.pmml4s.model;

import org.junit.Test;
import org.pmml4s.common.*;
import org.pmml4s.data.Series;
import org.pmml4s.metadata.AttributeType;
import org.pmml4s.metadata.DataField;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Examples how to use PMML4S in Java
 * <p>
 * !!! NOTE: these test cases are only available against scala 2.12 and 2.13.
 */
public class JModelTest {

    @Test
    public void testModelInJava() throws IOException {
        URL url = new URL("http://dmg.org/pmml/pmml_examples/KNIME_PMML_4.1_Examples/single_iris_dectree.xml");
        InputStream is = url.openStream();
        Model model = Model.fromInputStream(is);
        assertEquals("4.1", model.version());
        assertTrue(model.dVersion() == 4.1);
        assertEquals("KNIME", model.header().copyright().get());
        assertEquals("KNIME", model.header().application().get().name());
        assertEquals("2.8.0", model.header().application().get().version().get());
        assertTrue(model.dataDictionary().size() == 5);

        DataField f = model.dataDictionary().apply("sepal_length");
        assertEquals("sepal_length", f.name());
        assertTrue(f.dataType() == DataType.DOUBLE());
        assertTrue(f.opType() == OpType.CONTINUOUS());
        assertTrue(f.attrType() == AttributeType.CONTINUOUS());
        assertTrue(f.attribute().intervals()[0].equals(Interval.apply(4.3, 7.9, Closure.closedClosed())));

        DataField t = model.dataDictionary().apply("class");
        assertEquals("class", t.name());
        assertTrue(t.dataType() == DataType.STRING());
        assertTrue(t.opType() == OpType.NOMINAL());
        assertTrue(t.attrType() == AttributeType.CATEGORICAL());
        assertArrayEquals(new String[]{"Iris-setosa", "Iris-versicolor", "Iris-virginica"}, t.validValues());

        assertTrue(model.modelElement() == ModelElement.TREE_MODEL());
        assertEquals("DecisionTree", model.modelName().get());
        assertTrue(model.functionName() == MiningFunction.classification());
        assertTrue(model.algorithmName().isEmpty());
        assertTrue(model.isScorable());

        assertArrayEquals(new String[]{"sepal_length", "sepal_width", "petal_length", "petal_width"}, model.inputNames());
        assertTrue(model.inputFields().length == 4);
        assertArrayEquals(new String[]{"class"}, model.targetNames());
        assertEquals("class", model.targetName());
        assertEquals(1, model.targetFields().length);
        assertEquals("class", model.targetField().name());
        assertTrue(model.opType() == OpType.NOMINAL());
        assertArrayEquals(new String[]{"Iris-setosa", "Iris-versicolor", "Iris-virginica"}, model.classes());
        assertTrue(model.numClasses() == 3);
        assertTrue(model.outputSchema().length() == 6);
        assertTrue(model.isClassification());

        TreeModel tree = (TreeModel) model;
        assertTrue(tree.splitCharacteristic() == SplitCharacteristic.binarySplit());
        assertTrue(tree.missingValueStrategy() == MissingValueStrategy.lastPrediction());
        assertTrue(tree.noTrueChildStrategy() == NoTrueChildStrategy.returnNullPrediction());

        // Series with schema
        StructType schema = model.inputSchema();
        Series r = model.predict(Series.fromArray(new Object[]{5.1, 3.5, 1.4, 0.2}, schema));
        assertTrue(r.schema().equals(model.outputSchema()));
        assertArrayEquals(new Object[]{"Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1"}, r.toArray());

        Series r2 = model.predict(Series.fromArray(new Object[]{7, 3.2, 4.7, 1.4}, schema));
        assertTrue(r2.schema().equals(model.outputSchema()));
        assertArrayEquals(new Object[]{"Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3"}, r2.toArray());

        // Series without schema
        Series r3 = model.predict(Series.fromArray(new Object[]{5.1, 3.5, 1.4, 0.2}));
        assertTrue(r3.schema().equals(model.outputSchema()));
        assertArrayEquals(new Object[]{"Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1"}, r3.toArray());
        Series r4 = model.predict(Series.fromArray(new Object[]{7, 3.2, 4.7, 1.4}));
        assertTrue(r4.schema().equals(model.outputSchema()));
        assertArrayEquals(new Object[]{"Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3"}, r4.toArray());

        // Array
        Object[] r5 = model.predict(new Double[]{5.1, 3.5, 1.4, 0.2});
        assertArrayEquals(new Object[]{"Iris-setosa", 1.0, 1.0, 0.0, 0.0, "1"}, r5);
        Object[] r6 = model.predict(new Double[]{7., 3.2, 4.7, 1.4});
        assertArrayEquals(new Object[]{"Iris-versicolor", 0.9074074074074074, 0.0, 0.9074074074074074, 0.09259259259259259, "3"}, r6);

        // Map
        Map r7 = model.predict(new HashMap<String, Object>() {{
            put("sepal_length", 5.1);
            put("sepal_width", 3.5);
            put("petal_length", 1.4);
            put("petal_width", 0.2);
        }});
        System.out.println(r7.toString());
        assertEquals("Iris-setosa", r7.get("predicted_class"));
        Map r8 = model.predict(new HashMap<String, Object>() {{
            put("sepal_length", 7);
            put("sepal_width", 3.2);
            put("petal_length", 4.7);
            put("petal_width", 1.4);
        }});
        System.out.println(r8.toString());
        assertEquals("Iris-versicolor", r8.get("predicted_class"));
    }

    @Test
    public void testAccessCompoundPredicateFields() {
        InputStream is = this.getClass().getResourceAsStream("/models/rule/dmg_rule_simple.xml");
        Model model = Model.fromInputStream(is);

        assertTrue(model instanceof RuleSetModel);

        RuleSetModel ruleSetModel = (RuleSetModel) model;

        final Rule[] rules = ruleSetModel.ruleSet().rules();
        if (rules[0] instanceof SimpleRule) {
            SimpleRule rule = (SimpleRule) rules[0];

            if (rule.predicate() instanceof CompoundPredicate) {
                CompoundPredicate predicate = (CompoundPredicate)  rule.predicate();
                predicate.booleanOperator();
                assertEquals(CompoundPredicate.BooleanOperator$.MODULE$.and(), predicate.booleanOperator());
            }
        }
    }
}

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
package org.pmml4s

/**
 * PMML is a standard for XML documents which express trained instances of analytic models. The following classes of
 * model are addressed:
 *
 *  - Association Rules, implemented by [[org.pmml4s.model.AssociationModel]]
 *  - Baseline Models, ***NOT IMPLEMENTED***
 *  - Bayesian Network, ***NOT IMPLEMENTED***
 *  - Center-Based & Distribution-Based Clustering, implemented by [[org.pmml4s.model.ClusteringModel]]
 *  - Gaussian Process, ***NOT IMPLEMENTED***
 *  - General Regression, implemented by [[org.pmml4s.model.GeneralRegressionModel]]
 *  - k-Nearest Neighbors, implemented by [[org.pmml4s.model.NearestNeighborModel]]
 *  - Naive Bayes, implemented by [[org.pmml4s.model.NaiveBayesModel]]
 *  - Neural Networks, implemented by [[org.pmml4s.model.NeuralNetwork]]
 *  - Regression, implemented by [[org.pmml4s.model.RegressionModel]]
 *  - Ruleset, implemented by [[org.pmml4s.model.RuleSetModel]]
 *  - Scorecard, implemented by [[org.pmml4s.model.Scorecard]]
 *  - Sequences, ***NOT IMPLEMENTED***
 *  - Text, ***NOT IMPLEMENTED***
 *  - Time Series, ***NOT IMPLEMENTED***
 *  - Decision Trees, implemented by [[org.pmml4s.model.TreeModel]]
 *  - Support Vector Machine, implemented by [[org.pmml4s.model.SupportVectorMachineModel]]
 */
package object model {
}

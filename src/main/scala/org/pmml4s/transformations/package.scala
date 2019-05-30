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
 * At various places the mining models use simple functions in order to map user data to values that are easier to use
 * in the specific model. For example, neural networks internally work with numbers, usually in the range from 0 to 1.
 * Numeric input data are mapped to the range [0..1], and categorical fields are mapped to series of 0/1 indicators.
 *
 * PMML defines various kinds of simple data transformations:
 *
 *  - Normalization: map values to numbers, the input can be continuous or discrete.
 *  - Discretization: map continuous values to discrete values.
 *  - Value mapping: map discrete values to discrete values.
 *  - Text Indexing: derive a frequency-based value for a given term.
 *  - Functions: derive a value by applying a function to one or more parameters
 *  - Aggregation: summarize or collect groups of values, e.g., compute average.
 *  - Lag: use a previous value of the given input field.
 */
package object transformations {

}
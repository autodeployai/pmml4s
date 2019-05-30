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

import org.pmml4s.common.MixedEvaluator
import org.pmml4s.data.Series
import org.pmml4s.metadata.Field
import org.pmml4s.util.Utils

/**
 * Field references are simply pass-throughs to fields previously defined in the DataDictionary, a DerivedField, or a
 * result field. For example, they are used in clustering models in order to define center coordinates for fields that
 * don't need further normalization.
 *
 * A missing input will produce a missing result. The optional attribute mapMissingTo may be used to map a missing
 * result to the value specified by the attribute. If the attribute is not present, the result remains missing.
 */
class FieldRef(
                override val field: Field,
                val mapMissingTo: Option[Any] = None) extends FieldExpression with MixedEvaluator {
  override def eval(series: Series): Any = {
    val res = super.eval(series)
    if (Utils.isMissing(res)) {
      mapMissingTo.getOrElse(null)
    } else {
      res
    }
  }

  override def deeval(input: Any) = if (field.isDerivedField) {
    val df = field.asInstanceOf[DerivedField]
    df.deeval(input)
  } else {
    // itself
    input
  }
}

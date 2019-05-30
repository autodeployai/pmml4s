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

import java.io.File

import org.pmml4s.model.Model

import scala.io.Source

/**
 * An utility app to validate PMML model
 */
object Validator extends App {
  if (args.length == 0) {
    println("There is at least one parameter specified your PMML model.")
    System.exit(-1)
  }

  val path = args(0)
  val source = if (new File(path).exists) {
    Source.fromFile(new File(path))
  } else {
    Source.fromURL(path)
  }

  val model = Model(source)
  val info =
    s"""PMML version: ${model.version}
       |Application: ${
      model.header.application.map(x => x.name + x.version.map(y => s"(${y})").getOrElse("")).getOrElse("NA")
    }
       |Model type: ${model.modelElement}
       |Model name: ${model.modelName.getOrElse("NA")}
       |Function name: ${model.functionName}
       |Inputs: ${model.inputNames.mkString(", ")}
       |Targets: ${model.targetNames.mkString(", ")}
       |Outputs: ${model.outputFields.map(_.name).mkString(", ")}""".stripMargin
  println(info)
}
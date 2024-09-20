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
package org.pmml4s.common

import org.pmml4s.data.{DataVal, NullSeries, Series}
import org.pmml4s.metadata.Algorithm.Algorithm
import org.pmml4s.metadata.RankBasis.RankBasis
import org.pmml4s.metadata.RankOrder.RankOrder
import org.pmml4s.model.{AssociationRule, ModelElement}

import scala.collection.mutable

trait HasPredictedValue {
  def predictedValue: DataVal
}

trait MutablePredictedValue extends HasPredictedValue {
  var predictedValue: DataVal = DataVal.NULL

  def setPredictedValue(predictedValue: DataVal): this.type = {
    if (predictedValue == null) {
      this.predictedValue = DataVal.NULL
    } else {
      this.predictedValue = predictedValue
    }
    this
  }

  def setPredictedValue(predictedValue: Double): this.type = {
    this.predictedValue = DataVal.from(predictedValue)
    this
  }

  def evalPredictedValueByProbabilities(probabilities: Map[DataVal, Double]): this.type = {
    if (probabilities.nonEmpty) {
      setPredictedValue(probabilities.maxBy(_._2)._1)
    }
    this
  }

  def clear(): this.type = {
    this.predictedValue = DataVal.NULL
    this
  }
}

trait HasTransformedValue

trait HasSegment extends HasTransformedValue {
  def segment(id: String): Series
}

trait MutableSegment extends HasSegment {
  val segments: mutable.Map[String, Series] = mutable.HashMap.empty

  override def segment(id: String): Series = segments.getOrElse(id, NullSeries)

  def putSegment(id: String, segment: Series): this.type = {
    segments +=  (id -> segment)
    this
  }

  def clear(): this.type = {
    this.segments.clear()
    this
  }
}

trait HasDecision

trait HasPredictedDisplayValue {
  def predictedDisplayValue: String
}

trait MutablePredictedDisplayValue extends HasPredictedDisplayValue {
  var predictedDisplayValue: String = _

  def setPredictedDisplayValue(predictedDisplayValue: String): this.type = {
    this.predictedDisplayValue = predictedDisplayValue
    this
  }

  def clear(): this.type = {
    this.predictedDisplayValue = null
    this
  }
}

trait HasProbabilities {
  def probability(value: DataVal): Double

  def probabilities: Map[DataVal, Double]
}

trait MutableProbabilities extends HasProbabilities {
  var probabilities: Map[DataVal, Double] = Map.empty

  override def probability(value: DataVal): Double = probabilities.getOrElse(value, Double.NaN)

  def putProbability(value: DataVal, probability: Double): this.type = {
    probabilities = probabilities + (value -> probability)
    this
  }

  def setProbabilities(probabilities: Map[DataVal, Double]): this.type = {
    this.probabilities = probabilities
    this
  }

  def setProbabilities(probabilities: Array[(DataVal, Double)]): this.type = {
    this.probabilities = probabilities.toMap
    this
  }

  def clear(): this.type = {
    this.probabilities = Map.empty
    this
  }
}

trait HasResidual {
  def residual: Double
}

trait HasStandardError {
  def standardError: Double
}

trait HasEntityId {
  def entityId: DataVal
}

trait MutableEntityId extends HasEntityId {
  var entityId: DataVal = DataVal.NULL

  def setEntityId(entityId: DataVal): this.type = {
    this.entityId = entityId
    this
  }

  def setEntityId(entityId: String): this.type = {
    this.entityId = DataVal.from(entityId)
    this
  }

  def setEntityId(entityId: Int): this.type = {
    this.entityId = DataVal.from(entityId)
    this
  }

  def clear(): this.type = {
    this.entityId = DataVal.NULL
    this
  }
}

trait HasEntityIds {
  def entityId(rank: Int): DataVal
}

trait MutableEntityIds extends HasEntityIds {
  var entityIds: Array[DataVal] = Array.empty

  def entityId(rank: Int): DataVal =
    if (rank >= 1 && rank <= entityIds.length) entityIds(rank - 1) else DataVal.NULL

  def setEntitiesId(entityIds: Array[DataVal]): this.type = {
    this.entityIds = entityIds
    this
  }

  def size: Int = entityIds.length

  def clear(): this.type = {
    entityIds = Array.empty
    this
  }
}

trait HasAffinities {
  def affinity(id: DataVal): Double
}

trait MutableAffinities extends HasAffinities {
  var affinities: Map[DataVal, Double] = Map.empty

  override def affinity(id: DataVal): Double = affinities.getOrElse(id, Double.NaN)

  def setAffinities(affinities: Map[DataVal, Double]): this.type = {
    this.affinities = affinities
    this
  }

  def clear(): this.type = {
    affinities = Map.empty
    this
  }
}

trait HasWarning {
  def warning: String
}

trait HasReasonCode {
  def reasonCode: String
}

trait HasReasonCodes {
  def reasonCode(rank: Int): String
}

trait MutableReasonCodes extends HasReasonCodes {
  var reasonCodes: Array[String] = Array.empty

  override def reasonCode(rank: Int): String = if (rank >= 1 && rank <= reasonCodes.length) reasonCodes(rank - 1) else null

  def addReasonCode(reasonCode: String): this.type = {
    reasonCodes = reasonCodes :+ reasonCode
    this
  }

  def clear(): this.type = {
    reasonCodes = Array.empty
    this
  }
}

trait HasConfidence {
  def confidence: Double
}

trait MutableConfidence extends HasConfidence {
  var confidence: Double = Double.NaN

  def setConfidence(confidence: Double): this.type = {
    this.confidence = confidence
    this
  }

  def clear(): this.type = {
    confidence = Double.NaN
    this
  }
}

trait HasAssociationRules {
  def getRules(criterion: (Algorithm, RankBasis, RankOrder)): Array[AssociationRule]

  def getRule(criterion: (Algorithm, RankBasis, RankOrder), rank: Int): Option[AssociationRule] = {
    val idx = rank - 1
    val rs = getRules(criterion)
    if (idx >= 0 && idx < rs.length) Some(rs(idx)) else None
  }
}

trait ModelOutputs {
  def modelElement: ModelElement

  def clear(): this.type
}

trait MultiModelOutputs extends ModelOutputs {
  def get(target: String): Option[ModelOutputs]

  def apply(target: String): ModelOutputs = get(target).get

  def toSeq: Seq[(String, ModelOutputs)]
}

trait HasPredictedValueWithProbabilities extends HasPredictedValue with HasProbabilities

trait MutablePredictedValueWithProbabilities extends HasPredictedValueWithProbabilities
  with MutablePredictedValue
  with MutableProbabilities {
  def evalPredictedValueByProbabilities(): this.type = {
    if (probabilities.nonEmpty) {
      setPredictedValue(probabilities.maxBy(_._2)._1)
    }
    this
  }

  def evalPredictedValueByProbabilities(classes: Array[DataVal]): this.type = {
    if (probabilities.nonEmpty && probabilities.size == classes.length - 1) {
      probabilities = probabilities + (getMissingClass(classes, probabilities) -> (1.0 - probabilities.values.sum))
    }
    evalPredictedValueByProbabilities()
  }

  private def getMissingClass(classes: Array[DataVal], probabilities: Map[DataVal, Double]): DataVal = {
    for (cls <- classes) {
      if (!probabilities.contains(cls))
        return cls
    }
    null
  }

  override def clear(): this.type = {
    super[MutablePredictedValue].clear()
    super[MutableProbabilities].clear()
    this
  }
}

trait ClsOutputs extends ModelOutputs
  with MutablePredictedValueWithProbabilities
  with MutablePredictedDisplayValue {

  override def clear(): this.type = {
    super[MutablePredictedValueWithProbabilities].clear()
    super[MutablePredictedDisplayValue].clear()
    this
  }
}

trait RegOutputs extends ModelOutputs
  with MutablePredictedValue {

  override def clear(): this.type = {
    super[MutablePredictedValue].clear()
    this
  }
}

trait CluOutputs extends ModelOutputs
  with MutablePredictedValue
  with MutablePredictedDisplayValue
  with MutableEntityId
  with MutableAffinities {

  override def setPredictedValue(predictedValue: DataVal): this.type = {
    super.setPredictedValue(predictedValue)
    setEntityId(predictedValue)
  }

  override def clear(): this.type = {
    super[MutablePredictedValue].clear()
    super[MutablePredictedDisplayValue].clear()
    super[MutableEntityId].clear()
    super[MutableAffinities].clear()
    this
  }
}

trait KNNOutputs extends ModelOutputs
  with MutablePredictedValue
  with MutablePredictedDisplayValue
  with MutableEntityIds
  with MutableAffinities {

  override def clear(): this.type = {
    super[MutablePredictedValue].clear()
    super[MutablePredictedDisplayValue].clear()
    super[MutableEntityIds].clear()
    super[MutableAffinities].clear()
    this
  }
}

trait SegmentOutputs extends ModelOutputs
  with MutableSegment {

  override def clear(): this.type = {
    super[MutableSegment].clear()
    this
  }
}

trait MixedClsWithRegOutputs extends ClsOutputs with RegOutputs {
  override def clear(): this.type = {
    super[ClsOutputs].clear()
    super[RegOutputs].clear()
    this
  }
}

class GenericMultiModelOutputs extends MultiModelOutputs {
  private val map: mutable.Map[String, ModelOutputs] = mutable.HashMap.empty

  override def get(target: String): Option[ModelOutputs] = map.get(target)

  override def toSeq: Seq[(String, ModelOutputs)] = map.toSeq

  def put(target: String, outputs: ModelOutputs): this.type = {
    map.put(target, outputs)
    this
  }

  def getAs[T <: ModelOutputs](target: String): T = {
    map(target).asInstanceOf[T]
  }

  def getOrInsert[T <: ModelOutputs](target: String, defaultValue: => T): T = {
    map.getOrElseUpdate(target, defaultValue).asInstanceOf[T]
  }

  override def modelElement: ModelElement = ???

  override def clear(): this.type = {
    map.clear()
    this
  }
}
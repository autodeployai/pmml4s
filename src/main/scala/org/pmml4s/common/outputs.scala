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

import org.pmml4s.data.{NullSeries, Series}
import org.pmml4s.metadata.Algorithm.Algorithm
import org.pmml4s.metadata.RankBasis.RankBasis
import org.pmml4s.metadata.RankOrder.RankOrder
import org.pmml4s.model.AssociationRule

import scala.collection.mutable

trait HasPredictedValue {
  def predictedValue: Any
}

trait MutablePredictedValue extends HasPredictedValue {
  var predictedValue: Any = _

  def setPredictedValue(predictedValue: Any): this.type = {
    this.predictedValue = predictedValue
    this
  }

  def evalPredictedValueByProbabilities(probabilities: Map[Any, Double]): this.type = {
    if (probabilities.nonEmpty) {
      setPredictedValue(probabilities.maxBy(_._2)._1)
    }
    this
  }
}

trait HasTransformedValue

trait HasSegment extends HasTransformedValue {
  def segment(id: String): Series
}

trait MutableSegment extends HasSegment {
  var segments: Map[String, Series] = Map.empty

  override def segment(id: String): Series = segments.getOrElse(id, NullSeries)

  def putSegment(id: String, segment: Series): this.type = {
    segments = segments + (id -> segment)
    this
  }

  def putSegments(segments: Map[String, Series]): this.type = {
    this.segments = segments
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
}

trait HasProbabilities {
  def probability(value: Any): Double

  def probabilities: Map[Any, Double]
}

trait MutableProbabilities extends HasProbabilities {
  var probabilities: Map[Any, Double] = Map.empty

  override def probability(value: Any): Double = probabilities.getOrElse(value, Double.NaN)

  def putProbability(value: Any, probability: Double): this.type = {
    probabilities = probabilities + (value -> probability)
    this
  }

  def setProbabilities(probabilities: Map[Any, Double]): this.type = {
    this.probabilities = probabilities
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
  def entityId: Any
}

trait MutableEntityId extends HasEntityId {
  var entityId: Any = _

  def setEntityId(entityId: Any): this.type = {
    this.entityId = entityId
    this
  }
}

trait HasEntityIds {
  def entityId(rank: Int): Any
}

trait MutableEntityIds extends HasEntityIds {
  var entityIds: Array[Any] = Array.empty

  def entityId(rank: Int): Any = if (rank >= 1 && rank <= entityIds.length) entityIds(rank - 1) else null

  def setEntitiesId(entityIds: Array[Any]): this.type = {
    this.entityIds = entityIds
    this
  }

  def putEntityId(rank: Int, entityId: Any): this.type = {
    this.entityIds(rank - 1) = entityId
    this
  }

  def size: Int = entityIds.length

  def resize(len: Int): this.type = {
    entityIds = Array.ofDim(len)
    this
  }
}

trait HasAffinities {
  def affinity(id: Any): Double
}

trait MutableAffinities extends HasAffinities {
  var affinities: Map[Any, Double] = Map.empty

  override def affinity(id: Any): Double = affinities.getOrElse(id, Double.NaN)

  def putAffinity(id: Any, affinity: Double): this.type = {
    affinities = affinities + (id -> affinity)
    this
  }

  def setAffinities(affinities: Map[Any, Double]): this.type = {
    this.affinities = affinities
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
}

trait HasAssociationRules {
  def getRules(criterion: (Algorithm, RankBasis, RankOrder)): Array[AssociationRule]

  def getRule(criterion: (Algorithm, RankBasis, RankOrder), rank: Int): Option[AssociationRule] = {
    val idx = rank - 1
    val rs = getRules(criterion)
    if (idx >= 0 && idx < rs.length) Some(rs(idx)) else None
  }
}

trait ModelOutputs

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

  def evalPredictedValueByProbabilities(classes: Array[Any]): this.type = {
    if (probabilities.nonEmpty && probabilities.size == classes.length - 1) {
      putProbability(getMissingClass(classes, probabilities), 1.0 - probabilities.values.sum)
    }
    evalPredictedValueByProbabilities()
  }

  private def getMissingClass(classes: Array[Any], probabilities: Map[Any, Double]): Any = {
    for (cls <- classes) {
      if (!probabilities.contains(cls))
        return cls
    }
    null
  }
}

trait ClsOutputs extends ModelOutputs
  with MutablePredictedValueWithProbabilities
  with MutablePredictedDisplayValue

trait RegOutputs extends ModelOutputs
  with MutablePredictedValue

trait CluOutputs extends ModelOutputs
  with MutablePredictedValue
  with MutablePredictedDisplayValue
  with MutableEntityId
  with MutableAffinities {

  override def setPredictedValue(predictedValue: Any): this.type = {
    super.setPredictedValue(predictedValue)
    setEntityId(predictedValue)
  }
}

trait KNNOutputs extends ModelOutputs
  with MutablePredictedValue
  with MutablePredictedDisplayValue
  with MutableEntityIds
  with MutableAffinities

trait SegmentOutputs extends ModelOutputs
  with MutableSegment

trait MixedClsWithRegOutputs extends ClsOutputs with RegOutputs

class GenericMultiModelOutputs extends MultiModelOutputs {
  private val map = new mutable.HashMap[String, ModelOutputs]()

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
}
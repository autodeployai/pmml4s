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

import java.util.regex.{Matcher, Pattern}
import org.apache.commons.text.similarity.LevenshteinDistance
import org.pmml4s.common.{PmmlElement, Table}
import org.pmml4s.data.{DataVal, DoubleVal, Series, StringVal}
import org.pmml4s.metadata.Field
import org.pmml4s.transformations.CountHits.CountHits
import org.pmml4s.transformations.LocalTermWeights.LocalTermWeights
import org.pmml4s.util.Utils

import scala.util.control.Breaks._

/**
 * The TextIndex element fully configures how the text in textField should be processed and translated into a frequency
 * metric for a particular term of interest. The actual frequency metric to be returned is defined through the
 * localTermWeights attribute.
 */
class TextIndex(
                 val field: Field,
                 val expression: Expression,
                 val textIndexNormalizations: Array[TextIndexNormalization] = Array.empty,
                 val localTermWeights: LocalTermWeights = LocalTermWeights.termFrequency,
                 val isCaseSensitive: Boolean = false,
                 val maxLevenshteinDistance: Int = 0,
                 val countHits: CountHits = CountHits.allHits,
                 val wordSeparatorCharacterRE: String = "\\s+",
                 val tokenize: Boolean = true
               ) extends NumericFieldExpression {
  override def eval(series: Series): DoubleVal = {
    val text = field.get(series)
    val term = expression.eval(series)
    if (Utils.isMissing(text) || Utils.isMissing(term)) {
      DataVal.NaN
    } else {
      var textValue = text.toString
      textIndexNormalizations.foreach(x => {
        textValue = x.normalize(textValue, isCaseSensitive, maxLevenshteinDistance, wordSeparatorCharacterRE, tokenize)
      })

      val termValue = term.toString
      val (textTokens, termTokens) = if (tokenize)
        (textValue.split(wordSeparatorCharacterRE), termValue.split(wordSeparatorCharacterRE)) else (
        Array(textValue), Array(termValue)
      )

      val res: Double = if (textTokens.isEmpty || termTokens.isEmpty) {
        0.0
      } else {

        // leading and trailing punctuations should then be removed
        val textTokensPunctuationFree = textTokens.map(x => TextIndex.PUNCTUATION_PATTERN.matcher(x).replaceAll(""))
        val termTokensPunctuationFree = termTokens.map(x => TextIndex.PUNCTUATION_PATTERN.matcher(x).replaceAll(""))

        val textTokensTest = if (isCaseSensitive) textTokensPunctuationFree else textTokensPunctuationFree.map(_.toLowerCase)
        val termTokensTest = if (isCaseSensitive) termTokensPunctuationFree else termTokensPunctuationFree.map(_.toLowerCase)

        var frequency = 0
        var minDistance = Integer.MAX_VALUE
        var i = 0
        while (i <= textTokensTest.length - termTokensTest.length) {
          var sumDistance = 0
          var hit = true
          breakable {
            var j = 0
            while (j < termTokensTest.length) {
              val levenshteinDistance = new LevenshteinDistance(maxLevenshteinDistance - sumDistance)
              val distance = levenshteinDistance.apply(textTokensTest(i + j), termTokensTest(j))
              if (distance == -1) {
                hit = false
                break
              } else {
                sumDistance += distance
              }
              j += 1
            }
          }

          if (hit) {
            countHits match {
              case CountHits.allHits  => frequency += 1
              case CountHits.bestHits => if (sumDistance < minDistance) {
                minDistance = sumDistance
                frequency = 1
              } else if (sumDistance == minDistance) {
                frequency += 1
              }
            }
          }
          i += 1
        }
        localTermWeights match {
          case LocalTermWeights.termFrequency                    => frequency
          case LocalTermWeights.binary                           => if (frequency > 0) 1 else 0
          case LocalTermWeights.logarithmic                      => Math.log(1 + frequency)
          case LocalTermWeights.augmentedNormalizedTermFrequency => ???
        }
      }

      DataVal.from(res)
    }
  }
}


/**
 * - termFrequency: use the number of times the term occurs in the document (x = freqi).
 * - binary: use 1 if the term occurs in the document or 0 if it doesn't (x = χ(freqi)).
 * - logarithmic: take the logarithm (base 10) of 1 + the number of times the term occurs in the document.
 * (x = log(1 + freqi))
 * - augmentedNormalizedTermFrequency: this formula adds to the binary frequency a "normalized" component expressing the
 * frequency of a term relative to the highest frequency of terms observed in that document
 * (x = 0.5 * (χ(freqi) + (freqi / maxk(freqk))) )
 */
object LocalTermWeights extends Enumeration {
  type LocalTermWeights = Value
  val termFrequency, binary, logarithmic, augmentedNormalizedTermFrequency = Value
}

/**
 * - allHits: count all hits
 * - bestHits: count all hits with the lowest Levenshtein distance
 */
object CountHits extends Enumeration {
  type CountHits = Value
  val allHits, bestHits = Value
}

/**
 * A TextIndexNormalization element offers more advanced ways of normalizing text input into a more controlled
 * vocabulary that corresponds to the terms being used in invocations of this indexing function. The normalization
 * operation is defined through a translation table, specified through a TableLocator or InlineTable element.
 */
class TextIndexNormalization(
                              val table: Table,
                              val isCaseSensitive: Option[Boolean],
                              val maxLevenshteinDistance: Option[Int],
                              val wordSeparatorCharacterRE: Option[String],
                              val tokenize: Option[Boolean],
                              val inField: String = "string",
                              val outField: String = "stem",
                              val regexField: String = "regex",
                              val recursive: Boolean = false
                            ) extends PmmlElement {

  def normalize(
                 text: String,
                 isCaseSensitive: Boolean,
                 maxLevenshteinDistance: Int,
                 wordSeparatorCharacterRE: String,
                 tokenize: Boolean): String = {
    val (rowNum, _) = table.dim
    var result = text
    val isCaseSensitiveUsed = this.isCaseSensitive.getOrElse(isCaseSensitive)

    if (recursive) {
      var stop = false
      breakable {
        while (!stop) {
          var i = 0
          while (i < rowNum) {
            val previous = result
            val row = table(i)
            val in = row(inField).toString
            val out = row(outField).toString
            result = row.get(regexField) match {
              case Some(StringVal("true")) => {
                previous.replaceAll(in, out)
              }
              case _            => {
                Pattern.compile(in, if (isCaseSensitiveUsed) Pattern.LITERAL else Pattern.LITERAL | Pattern.CASE_INSENSITIVE).
                  matcher(previous).replaceAll(Matcher.quoteReplacement(out))
              }
            }

            if (result != previous) {
              stop = true
              break
            }
            i += 1
          }
        }
      }
    } else {
      var i = 0
      while (i < rowNum) {
        val row = table(i)
        val in = row(inField).toString
        val out = row(outField).toString
        result = row.get(regexField) match {
          case Some(StringVal("true")) => {
            result.replaceAll(in, out)
          }
          case _            => {
            Pattern.compile(in, if (isCaseSensitiveUsed) Pattern.LITERAL else Pattern.LITERAL | Pattern.CASE_INSENSITIVE).
              matcher(result).replaceAll(Matcher.quoteReplacement(out))
          }
        }
        i += 1
      }
    }

    result
  }
}

object TextIndex {

  import java.util.regex.Pattern

  val PUNCTUATION_PATTERN: Pattern = Pattern.compile("\\p{Punct}")
}

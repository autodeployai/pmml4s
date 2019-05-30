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

import java.util.Arrays

import org.pmml4s.common.MatrixKind.MatrixKind

import scala.collection.mutable


/**
 * Trait for a matrix.
 */
trait Matrix extends PmmlElement {
  def apply(i: Int, j: Int): Double

  def default: Double = 0.0

  def diagDefault: Option[Double]

  def offDiagDefault: Option[Double]

  def nbRows: Int

  def nbCols: Int

  def kind: MatrixKind
}

/**
 * The content is just one array of numbers representing the diagonal values.
 */
class DiagonalMatrix(val values: Array[Double], val offDiagDefault: Option[Double]) extends Matrix {
  override def apply(i: Int, j: Int): Double = if (i == j) values(i) else offDiagDefault.getOrElse(default)

  override def nbRows: Int = values.length

  override def nbCols: Int = values.length

  override def kind: MatrixKind = MatrixKind.diagonal

  override def diagDefault: Option[Double] = None
}

/**
 * The content must be represented by Arrays. The first array contains the matrix element M(0,0), the second array
 * contains M(1,0), M(1,1), and so on (that is the lower left triangle). Other elements are defined by symmetry.
 */
class SymmetricMatrix(val values: Array[Array[Double]]) extends Matrix {
  override def apply(i: Int, j: Int): Double = if (i > j) values(j)(i) else values(i)(j)

  override def nbRows: Int = values.length

  override def nbCols: Int = values(nbRows - 1).length

  override def kind: MatrixKind = MatrixKind.symmetric

  override def diagDefault: Option[Double] = None

  override def offDiagDefault: Option[Double] = None
}

/**
 * Dense matrix
 */
class DenseMatrix(val values: Array[Array[Double]]) extends Matrix {
  override def apply(i: Int, j: Int): Double = values(i)(j)

  override def nbRows: Int = values.length

  override def nbCols: Int = values(0).length

  override def kind: MatrixKind = MatrixKind.any

  override def diagDefault: Option[Double] = None

  override def offDiagDefault: Option[Double] = None
}

/**
 * Column-major sparse matrix.
 */
class SparseMatrix(val nbRows: Int,
                   val nbCols: Int,
                   val colPtrs: Array[Int],
                   val rowIndices: Array[Int],
                   val values: Array[Double],
                   val diagDefault: Option[Double],
                   val offDiagDefault: Option[Double]) extends Matrix {

  override def apply(i: Int, j: Int): Double = {
    val ind = index(i, j)
    if (ind < 0) default(i, j) else values(ind)
  }

  override def kind: MatrixKind = MatrixKind.any

  def index(i: Int, j: Int): Int = {
    Arrays.binarySearch(rowIndices, colPtrs(j), colPtrs(j + 1), i)
  }

  def default(i: Int, j: Int): Double = if (i == j) diagDefault.getOrElse(default) else offDiagDefault.getOrElse(default)
}

object SparseMatrix {

  def fromCells(nbRows: Int, nbCols: Int, matCells: Array[MatCell], diagDefault: Option[Double], offDiagDefault: Option[Double]): SparseMatrix = {
    val sortedEntries = matCells.sortBy(v => (v.col, v.row))
    val numEntries = sortedEntries.size
    if (sortedEntries.nonEmpty) {
      // Since the entries are sorted by column index, we only need to check the first and the last.
      for (col <- Seq(sortedEntries.head.col, sortedEntries.last.col)) {
        require(col >= 0 && col < nbCols, s"Column index out of range [0, $nbCols): $col.")
      }
    }
    val colPtrs = new Array[Int](nbCols + 1)
    val rowIndices = mutable.ArrayBuilder.make[Int]
    rowIndices.sizeHint(numEntries)
    val values = mutable.ArrayBuilder.make[Double]
    values.sizeHint(numEntries)
    var nnz = 0
    var prevCol = 0
    var prevRow = -1
    var prevVal = 0.0
    // Append a dummy entry to include the last one at the end of the loop.
    (sortedEntries.view :+ MatCell(nbRows, nbCols, 1.0)).foreach { case MatCell(i, j, v) =>
      if (i == prevRow && j == prevCol) {
        prevVal += v
      } else {
        if (prevVal != 0) {
          require(prevRow >= 0 && prevRow < nbRows,
            s"Row index out of range [0, $nbRows): $prevRow.")
          nnz += 1
          rowIndices += prevRow
          values += prevVal
        }
        prevRow = i
        prevVal = v
        while (prevCol < j) {
          colPtrs(prevCol + 1) = nnz
          prevCol += 1
        }
      }
    }
    new SparseMatrix(nbRows, nbCols, colPtrs, rowIndices.result(), values.result(), diagDefault, offDiagDefault)
  }
}

case class MatCell(val row: Int, val col: Int, val value: Double)

object MatrixKind extends Enumeration {
  type MatrixKind = Value
  val diagonal, symmetric, any = Value
}
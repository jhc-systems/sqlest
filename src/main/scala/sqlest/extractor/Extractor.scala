/*
 * Copyright 2014 JHC Systems Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sqlest.extractor

import java.sql.ResultSet
import org.joda.time.DateTime
import scala.collection.immutable._
import sqlest.ast._
import sqlest.util._

sealed trait Extractor[A] extends Logging {
  type Accumulator
  def columns: List[AliasedColumn[_]]
  def nonOptionalColumns: List[AliasedColumn[_]]

  type SingleResult

  def extractOne(row: ResultSet): Option[SingleResult]
  def extractAll(row: ResultSet): List[SingleResult]

  def initialize(row: ResultSet): Accumulator
  def accumulate(row: ResultSet, accumulator: Accumulator): Accumulator
  def emit(accumulator: Accumulator): A

  def map[B](func: A => B) = MappedExtractor(this, func)
  def asOption = OptionExtractor(this)
}

trait SingleExtractor[A] extends Extractor[A] {
  final type SingleResult = A

  final def extractOne(row: ResultSet): Option[A] =
    asList.extractOne(row)

  final def extractAll(row: ResultSet): List[A] =
    asList.extractAll(row)

  def asList = ListExtractor(this)
  def groupBy[B](groupBy: Extractor[B]) = GroupedExtractor(this, groupBy)
}

trait MultiExtractor[A] extends Extractor[List[A]] {
  type Accumulator <: Traversable[_]

  final type SingleResult = A

  final def extractOne(row: ResultSet): Option[A] = {
    if (row.isFirst || row.isBeforeFirst && row.next) {
      var accumulator = initialize(row)

      while (row.next && accumulator.size == 1)
        accumulator = accumulate(row, accumulator)

      emit(accumulator).headOption
    } else None
  }

  final def extractAll(row: ResultSet): List[A] = {
    if (row.isFirst || row.isBeforeFirst && row.next) {
      var accumulator = initialize(row)

      while (row.next)
        accumulator = accumulate(row, accumulator)

      emit(accumulator)
    } else Nil
  }
}

/**
 * Extractor that always returns the same value
 */
case class ConstantExtractor[A](value: A) extends SingleExtractor[A] {
  type Accumulator = A
  val columns = Nil
  val nonOptionalColumns = Nil

  def initialize(row: ResultSet) = value
  def accumulate(row: ResultSet, accumulator: A) = value
  def emit(accumulator: A) = accumulator
}

/**
 * Extractor that emits the values for a single `column`.
 */
case class ColumnExtractor[A](column: AliasedColumn[A]) extends SingleExtractor[A] {
  type Accumulator = A
  val columns = List(column)
  val nonOptionalColumns =
    column.columnType match {
      case _: OptionColumnType[_] => Nil
      case _ => List(column)
    }

  def initialize(row: ResultSet) = read(row, column.columnType)

  def accumulate(row: ResultSet, accumulator: A) = read(row, column.columnType)

  def emit(accumulator: A) = accumulator

  private def read[B](row: ResultSet, columnType: ColumnType[B]): B =
    columnType match {
      case BooleanColumnType => row getBoolean column.columnAlias
      case IntColumnType => row getInt column.columnAlias
      case LongColumnType => row getLong column.columnAlias
      case DoubleColumnType => row getDouble column.columnAlias
      case BigDecimalColumnType => BigDecimal(row getBigDecimal column.columnAlias)
      case StringColumnType => row getString column.columnAlias
      case DateTimeColumnType => new DateTime(row getDate column.columnAlias)
      case OptionColumnType(base) => Option(read(row, base))
        .filterNot(_ => row.wasNull)
        .asInstanceOf[B] // ugly cast but it will always work
      case mapped: MappedColumnType[B, _] => mapped.read(read(row, mapped.baseType))
    }

  def as[B](func: A => B) = MappedExtractor(this, func)
}

trait ProductExtractor[A <: Product] extends SingleExtractor[A] {
  def innerExtractors: List[Extractor[_]]
}

/**
 * An extractor that behaves as `inner` but pipes its `emitted` values through `func`.
 */
case class MappedExtractor[A, B](inner: Extractor[A], func: A => B) extends SingleExtractor[B] {
  type Accumulator = inner.Accumulator
  val columns = inner.columns
  val nonOptionalColumns = inner.nonOptionalColumns

  def initialize(row: ResultSet) = inner.initialize(row)

  def accumulate(row: ResultSet, accumulator: inner.Accumulator) = inner.accumulate(row, accumulator)

  def emit(accumulator: inner.Accumulator) = func(inner.emit(accumulator))
}

/**
 * An extractor that returns `None` if all of the columns in the
 * `inner` extractor are `null` in the ResultSet.
 *
 * If any underlying column is non-`null`, this returns `Some`
 * of `inner`'s result.
 */
case class OptionExtractor[A](inner: Extractor[A]) extends SingleExtractor[Option[A]] {
  type Accumulator = Option[inner.Accumulator]
  val columns = inner.columns
  val nonOptionalColumns = Nil

  private def containsNotNulls(row: ResultSet) =
    inner.nonOptionalColumns.isEmpty || !inner.nonOptionalColumns.exists { column =>
      row getObject column.columnAlias
      row.wasNull
    }

  def initialize(row: ResultSet) =
    if (containsNotNulls(row))
      Some(inner.initialize(row))
    else
      None

  def accumulate(row: ResultSet, accumulator: Option[inner.Accumulator]) =
    if (containsNotNulls(row))
      accumulator.map(accumulator => inner.accumulate(row, accumulator))
    else
      None

  def emit(accumulator: Option[inner.Accumulator]) = accumulator map (inner.emit)
}

/**
 * An extractor that accumulates results into a list.
 */
case class ListExtractor[A](inner: Extractor[A]) extends MultiExtractor[A] {
  type Accumulator = Queue[A]
  val columns = inner.columns
  val nonOptionalColumns = inner.nonOptionalColumns

  // Rows where all the columns are null should not be extracted but instead should not add
  // an item to the left. This allows us to support left joins which return an empty list
  private val innerOptionExtractor = OptionExtractor(inner)
  private def readInnerOptionValue(row: ResultSet) = innerOptionExtractor.emit(innerOptionExtractor.initialize(row))

  def initialize(row: ResultSet) = readInnerOptionValue(row) match {
    case Some(innerValue) => Queue(innerValue)
    case None => Queue()
  }

  def accumulate(row: ResultSet, accumulator: Queue[A]) = readInnerOptionValue(row) match {
    case Some(innerValue) => accumulator :+ innerValue
    case None => accumulator
  }

  def emit(accumulator: Queue[A]) = accumulator.toList
}

/**
 * An extractor that accumulates results with the same groupBy value into the same value
 */
case class GroupedExtractor[A, B](inner: Extractor[A], groupBy: Extractor[B]) extends MultiExtractor[A] {
  // Consider using a tuple of a Queue and a HashMap as the Accumulator for efficiency
  type Accumulator = ListMap[B, inner.Accumulator]
  val columns = (inner.columns ++ groupBy.columns).distinct
  val nonOptionalColumns = inner.nonOptionalColumns

  def initialize(row: ResultSet) =
    ListMap(groupBy.emit(groupBy.initialize(row)) -> inner.initialize(row))

  def accumulate(row: ResultSet, accumulator: ListMap[B, inner.Accumulator]) = {
    val groupByKey = groupBy.emit(groupBy.initialize(row))

    val newInnerAccumulator = accumulator.get(groupByKey) match {
      case Some(innerAccumulator) => inner.accumulate(row, innerAccumulator)
      case None => inner.initialize(row)
    }

    accumulator + (groupByKey -> newInnerAccumulator)
  }

  def emit(accumulator: ListMap[B, inner.Accumulator]) = accumulator.values.map(inner.emit).toList
}

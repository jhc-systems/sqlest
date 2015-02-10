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

import org.joda.time.DateTime
import scala.collection.immutable.{ Queue, ListMap }

sealed trait Extractor[Row, A] {
  type Accumulator

  type SingleResult

  def extractHeadOption(row: Iterable[Row]): Option[SingleResult]
  def extractAll(row: Iterable[Row]): List[SingleResult]

  def initialize(row: Row): Accumulator
  def accumulate(accumulator: Accumulator, row: Row): Accumulator
  // An Option is emitted to represent null values
  // If a null value is not handled by an OptionExtractor then an exception will be thrown
  def emit(accumulator: Accumulator): Option[A]

  protected def checkNullValueAndGet[T](t: Option[T]) =
    t.getOrElse(throw new NullPointerException("Tried to extract a null value without an OptionExtractor"))
}

sealed trait SingleRowExtractor[Row, A] extends Extractor[Row, A] {
  final type SingleResult = A

  final def extractHeadOption(rows: Iterable[Row]): Option[A] = {
    val rowIterator = rows.iterator
    if (rowIterator.hasNext) {
      Some(checkNullValueAndGet(emit(initialize(rowIterator.next))))
    } else None
  }

  final def extractAll(rows: Iterable[Row]): List[A] = {
    val rowIterator = rows.iterator
    if (rowIterator.hasNext) {
      var accumulator = Queue(checkNullValueAndGet(emit(initialize(rowIterator.next))))

      while (rowIterator.hasNext)
        accumulator = accumulator :+ checkNullValueAndGet(emit(initialize(rowIterator.next)))

      accumulator.toList
    } else Nil
  }

  def map[B](func: A => B) = MappedExtractor(this, func)
  def asOption = OptionExtractor(this)
  def asList = ListMultiRowExtractor(this)
  def groupBy[B](groupBy: Extractor[Row, B]) = GroupedMultiRowExtractor(this, groupBy)
}

sealed trait MultiRowExtractor[Row, A] extends Extractor[Row, List[A]] {
  type Accumulator <: Traversable[_]

  final type SingleResult = A

  final def extractHeadOption(rows: Iterable[Row]): Option[A] = {
    val rowIterator = rows.iterator
    if (rowIterator.hasNext) {
      var accumulator = initialize(rowIterator.next)

      while (rowIterator.hasNext && accumulator.size == 1)
        accumulator = accumulate(accumulator, rowIterator.next)

      checkNullValueAndGet(emit(accumulator)).headOption
    } else None
  }

  final def extractAll(rows: Iterable[Row]): List[A] = {
    val rowIterator = rows.iterator
    if (rowIterator.hasNext) {
      var accumulator = initialize(rowIterator.next)

      while (rowIterator.hasNext)
        accumulator = accumulate(accumulator, rowIterator.next)

      checkNullValueAndGet(emit(accumulator))
    } else Nil
  }
}

/**
 * Extractor that always returns the same value
 */
case class ConstantExtractor[Row, A](value: A) extends SingleRowExtractor[Row, A] {
  type Accumulator = A

  def initialize(row: Row) = value
  def accumulate(accumulator: A, row: Row) = value
  def emit(accumulator: A) = Some(accumulator)
}

/**
 * Extractor that emits the values for a single cell.
 */
trait CellExtractor[Row, A] extends SingleRowExtractor[Row, A] {
  type Accumulator = Option[A]

  def initialize(row: Row) = read(row)

  def accumulate(accumulator: Accumulator, row: Row) = read(row)

  def emit(accumulator: Accumulator) = accumulator

  def read(row: Row): Option[A]
}

/**
 * An extractor acts as a base type for extracting Product types
 */
trait ProductExtractor[Row, A <: Product] extends SingleRowExtractor[Row, A] {
  def innerExtractors: List[Extractor[Row, _]]
}

/**
 * Add ProductExtractorNames to a ProductExtractor to allow
 * inner extractors to be found by name
 */
trait ProductExtractorNames {
  this: ProductExtractor[_, _] =>
  def innerExtractorNames: List[String]
}

/**
 * An extractor that behaves as `inner` but pipes its `emitted` values through `func`.
 */
case class MappedExtractor[Row, A, B](inner: Extractor[Row, A], func: A => B) extends SingleRowExtractor[Row, B] {
  type Accumulator = inner.Accumulator

  def initialize(row: Row) = inner.initialize(row)

  def accumulate(accumulator: inner.Accumulator, row: Row) = inner.accumulate(accumulator, row)

  def emit(accumulator: inner.Accumulator) = inner.emit(accumulator).map(func)
}

/**
 * An extractor that aggregates results from a seq of extractors into a seq.
 */
case class SeqExtractor[Row, A](extractors: Seq[Extractor[Row, A]]) extends SingleRowExtractor[Row, Seq[A]] {
  type Accumulator = Seq[Any]

  def initialize(row: Row): Accumulator = extractors.map(_.initialize(row))

  def accumulate(accumulators: Accumulator, row: Row) = extractors.zip(accumulators).map {
    case (extractor, accumulator) => extractor.accumulate(accumulator.asInstanceOf[extractor.Accumulator], row)
  }

  def emit(accumulators: Accumulator) = {
    val innerEmitted = extractors.zip(accumulators).map {
      case (extractor, accumulator) => extractor.emit(accumulator.asInstanceOf[extractor.Accumulator])
    }
    if (innerEmitted.exists(_.isEmpty)) None else Some(innerEmitted.map(_.get))
  }
}

/**
 * An extractor that returns `None` if all of the cells in the
 * `inner` extractor are `null` in the row.
 *
 * If any underlying cell is non-`null`, this returns `Some`
 * of `inner`'s result.
 */
case class OptionExtractor[Row, A](inner: Extractor[Row, A]) extends SingleRowExtractor[Row, Option[A]] {
  type Accumulator = inner.Accumulator

  def initialize(row: Row) = inner.initialize(row)

  def accumulate(accumulator: inner.Accumulator, row: Row) = inner.accumulate(accumulator, row)

  def emit(accumulator: inner.Accumulator) = Some(inner.emit(accumulator))
}

/**
 * An extractor that accumulates results into a list.
 */
case class ListMultiRowExtractor[Row, A](inner: Extractor[Row, A]) extends MultiRowExtractor[Row, A] {
  type Accumulator = Queue[Option[A]]

  def initialize(row: Row) = Queue(inner.emit(inner.initialize(row)))

  def accumulate(accumulator: Queue[Option[A]], row: Row) = accumulator :+ inner.emit(inner.initialize(row))

  // In a left join either all row are full or all rows are null.
  // These are the valid accumulators that will return a list
  def emit(accumulator: Queue[Option[A]]) = {
    val noRowsEmpty = accumulator.forall(!_.isEmpty)
    val allRowsEmpty = accumulator.forall(_.isEmpty)
    if (noRowsEmpty)
      Some(accumulator.map(_.get).toList)
    else if (allRowsEmpty)
      Some(Nil)
    else
      None
  }
}

/**
 * An extractor that accumulates results with the same groupBy value into the same value
 */
case class GroupedMultiRowExtractor[Row, A, B](inner: Extractor[Row, A], groupBy: Extractor[Row, B]) extends MultiRowExtractor[Row, A] {
  // Consider using a tuple of a Queue and a HashMap as the Accumulator for efficiency
  type Accumulator = ListMap[B, inner.Accumulator]

  def initialize(row: Row) =
    ListMap(checkNullValueAndGet(groupBy.emit(groupBy.initialize(row))) -> inner.initialize(row))

  def accumulate(accumulator: ListMap[B, inner.Accumulator], row: Row) = {
    val groupByKey = checkNullValueAndGet(groupBy.emit(groupBy.initialize(row)))

    val newInnerAccumulator = accumulator.get(groupByKey) match {
      case Some(innerAccumulator) => inner.accumulate(innerAccumulator, row)
      case None => inner.initialize(row)
    }

    accumulator + (groupByKey -> newInnerAccumulator)
  }

  def emit(accumulator: ListMap[B, inner.Accumulator]) = Some(accumulator.values.map(inner.emit).toList.map(checkNullValueAndGet))
}

object Extractor {
  implicit class ExtractorOps(extractor: Extractor[_, _]) {
    def findCellExtractor(path: String) = ExtractorFinder(extractor, path)
  }
}

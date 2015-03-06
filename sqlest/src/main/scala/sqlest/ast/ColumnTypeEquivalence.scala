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

package sqlest.ast

/**
 * Typeclass representing the equivalence of two column types.
 * For example: `1 === 2` is a valid sqlest expression but `1 === "2"` is not.
 *
 * The main requirement for this class comes from `OptionColumnTypes`. sqlest is relaxed
 * about allowing direct comparisons between optional and non-optional columns.
 * For example, `1 === Some(2)` is considered valid.
 *
 * The second requirement is that different numeric column types should be comparable
 */
case class ColumnTypeEquivalence[A, B](leftOption: Boolean, rightOption: Boolean)

object ColumnTypeEquivalence extends LowPriorityImplicits {
  implicit def mappedColumnTypeEquivalence[A, B](implicit left: MappedColumnType[A, B], right: MappedColumnType[A, B]) =
    ColumnTypeEquivalence[A, A](false, false)

  implicit def leftOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[Option[A]], right: ColumnType[B], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    ColumnTypeEquivalence[Option[A], B](true, false)

  implicit def rightOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[A], right: ColumnType[Option[B]], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    ColumnTypeEquivalence[A, Option[B]](false, true)

  implicit def bothOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[Option[A]], right: ColumnType[Option[B]], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    ColumnTypeEquivalence[Option[A], Option[B]](true, true)
}

trait LowPriorityImplicits {
  // It's possible mappedColumnTypeEquivalence to clash with these, if for example TrimmedStringColumnType
  // has been implicitly put in scope. Make these lower priority

  implicit def nonNumericEquivalence[A](implicit left: NonNumericColumnType[A], right: NonNumericColumnType[A]) =
    ColumnTypeEquivalence[A, A](false, false)

  implicit def numericEquivalence[A, B](implicit left: NumericColumnType[A], right: NumericColumnType[B]) =
    ColumnTypeEquivalence[A, B](false, false)
}

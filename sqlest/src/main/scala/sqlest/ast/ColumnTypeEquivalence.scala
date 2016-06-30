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

trait ColumnTypeEquivalence[A, B]

object ColumnTypeEquivalence extends LowPriorityImplicits {
  implicit def leftOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[Option[A]], right: ColumnType[B], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    new ColumnTypeEquivalence[Option[A], B] {}

  implicit def rightOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[A], right: ColumnType[Option[B]], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    new ColumnTypeEquivalence[A, Option[B]] {}

  implicit def bothOptionColumnTypeEquivalence[A, B](implicit left: ColumnType[Option[A]], right: ColumnType[Option[B]], columnTypeEquivalence: ColumnTypeEquivalence[A, B]) =
    new ColumnTypeEquivalence[Option[A], Option[B]] {}

  /**
   * This function performs 2 functions on a combination of a database column and a literal/constant column
   *   1. Sets the column type on the literal column to that of the database column so it correctly written
   *   2. Wraps/unwraps Option literals so they are the same as the database column
   *
   * For 2 database columns it checks to see if the underlying columnTypes are compatible. Ideally this would
   * be taken care of by the ColumnTypeEquivalence typeclass but unfortunately there often isn't enough information
   * available at compile time
   */
  def alignColumnTypes[A, B](left: Column[A], right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]): (Column[_], Column[_]) = {
    def isLiteral(column: Column[_]): Boolean = column match {
      case _: LiteralColumn[_] => true
      case _: ConstantColumn[_] => true
      case _ => false
    }

    def convertLiteral[C, L](columnType: ColumnType[C], literal: Column[L]): Column[_] =
      literal match {
        case LiteralColumn(value) => LiteralColumn[C](alignOptionLiteral(columnType, value).asInstanceOf[C])(columnType)
        case ConstantColumn(value) => ConstantColumn[C](alignOptionLiteral(columnType, value).asInstanceOf[C])(columnType)
        case _ => sys.error("")
      }

    def alignOptionLiteral[C, L](columnType: ColumnType[C], value: L): Any = (columnType, value) match {
      case (_: OptionColumnType[_, _], _: Option[_]) => value
      case (_: OptionColumnType[_, _], _) => Some(value)
      case (_, Some(inner)) => inner
      case (_, None) => throw new AssertionError("Cannot compare a non-Option column to None")
      case _ => value
    }

    def checkCompatible[A, B](left: ColumnType[A], right: ColumnType[B]): Unit = {
      def fail = throw new AssertionError(s"Incompatible column comparison - $left, $right")

      (left, right) match {
        case (leftOption: OptionColumnType[_, _], rightOption: OptionColumnType[_, _]) =>
          checkCompatible(leftOption.innerColumnType, rightOption.innerColumnType)
        case (leftOption: OptionColumnType[_, _], right) =>
          checkCompatible(leftOption.innerColumnType, right)
        case (left, rightOption: OptionColumnType[_, _]) =>
          checkCompatible(left, rightOption.innerColumnType)
        case (leftMapped: MappedColumnType[_, _], rightMapped: MappedColumnType[_, _]) =>
          checkCompatible(leftMapped.innerColumnType, rightMapped.innerColumnType)
        case (leftMapped: MappedColumnType[_, _], right) =>
          checkCompatible(leftMapped.innerColumnType, right)
        case (left, rightMapped: MappedColumnType[_, _]) =>
          checkCompatible(left, rightMapped.innerColumnType)
        case (_: NumericColumnType[_], _: NumericColumnType[_]) =>
        case (left, right) if left != right => fail
        case _ =>
      }
    }

    (isLiteral(left), isLiteral(right)) match {
      case (true, true) => (left, right)
      case (false, true) => (left, convertLiteral(left.columnType, right))
      case (true, false) => (convertLiteral(right.columnType, left), right)
      case (false, false) =>
        checkCompatible(left.columnType, right.columnType)
        (left, right)
    }
  }
}

trait LowPriorityImplicits {
  // The implicit *OptionColumnTypeEquivalences will clash with this, ensure this is lower priority so options are correctly handled
  implicit def nonNumericEquivalence[A, B](implicit left: ColumnType[A] { type Database = B }, right: ColumnType[A] { type Database = B }) =
    new ColumnTypeEquivalence[A, A] {}

  implicit def numericEquivalence[A, B](implicit left: NumericColumnType[A], right: NumericColumnType[B]) =
    new ColumnTypeEquivalence[A, B] {}
}

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

import org.joda.time.DateTime
import scala.reflect.runtime.{ universe => ru }

/**
 * Object representing a mapping between a Scala data type and an underlying SQL data type.
 * Column types are broadly divided into three categories:
 *
 *  - `BaseColumnTypes` represent Scala data types that can be directly mapped to SQL types;
 *  - `OptionColumnTypes` represent Scala `Option` types that are mapped to nullable SQL types;
 *  - `MappedColumnTypes` represent arbitrary mappings between Scala types and SQL types.
 */
sealed trait ColumnType[+A] {
  def typeTag: ru.TypeTag[_]
}

/** Object representing a basic (non-optional) column type. */
sealed trait BaseColumnType[+A] extends ColumnType[A]

sealed trait NumericBaseColumnType[+A] extends BaseColumnType[A]
sealed trait NonNumericBaseColumnType[+A] extends BaseColumnType[A]

case object IntColumnType extends NumericBaseColumnType[Int] {
  val typeTag = ru.typeTag[Int]
}
case object LongColumnType extends NumericBaseColumnType[Long] {
  val typeTag = ru.typeTag[Long]
}
case object DoubleColumnType extends NumericBaseColumnType[Double] {
  val typeTag = ru.typeTag[Double]
}
case object BigDecimalColumnType extends NumericBaseColumnType[BigDecimal] {
  val typeTag = ru.typeTag[BigDecimal]
}

case object BooleanColumnType extends NonNumericBaseColumnType[Boolean] {
  val typeTag = ru.typeTag[Boolean]
}
case object StringColumnType extends NonNumericBaseColumnType[String] {
  val typeTag = ru.typeTag[String]
}
case object DateTimeColumnType extends NonNumericBaseColumnType[DateTime] {
  val typeTag = ru.typeTag[DateTime]
}

/**
 * Object representing an nullable SQL column type that is mapped to an `Option` in Scala.
 *
 * For every `OptionColumnType` there is an underlying `BaseColumnType`.
 */
case class OptionColumnType[+A](baseType: BaseColumnType[A]) extends ColumnType[Option[A]] {
  val typeTag = ru.typeTag[Option[_]]
}

/**
 * Object representing a custom column type `A` with an underlying base type `B`.
 *
 * For every `MappedColumnType` there is an underlying `BaseColumnType`.
 */
abstract class MappedColumnType[A: ru.TypeTag, B] extends ColumnType[A] {
  def baseType: BaseColumnType[B]
  def read(database: B): A
  def write(value: A): B
  val typeTag = ru.typeTag[A]
}

object MappedColumnType {
  /**
   * Convenience method for constructing a `MappedColumnType` from a pair of
   * bidirectional mapping functions and an implicitly provided `BaseColumnType`.
   */
  def apply[A: ru.TypeTag, B: BaseColumnType](r: B => A, w: A => B) = new MappedColumnType[A, B] {
    val baseType = implicitly[BaseColumnType[B]]
    def read(database: B) = r(database)
    def write(value: A) = w(value)
  }
}

/**
 * Class representing the equivalence of two column types. Instances of this class
 * are made implicitly available via `ColumnTypeEquivalences` to allow the compiler
 * to type-check the expressions produced by traits in the `sqlest.ast.syntax` package.
 * For example, `1 === 2` is a valid SQLest expression but `1 === "2"` is not.
 *
 * The main requirement for this class comes from `OptionColumnTypes`. SQLest is relaxed
 * about allowing direct comparisons between optional and non-optional columns.
 * For example, `1 === Some(2)` is considered valid.
 */
case class ColumnTypeEquivalence[A, B](left: ColumnType[A], right: ColumnType[B])

/**
 * Trait providing implicit constructors for `BaseColumnTypes` and `OptionColumnTypes`.
 */
trait BaseColumnTypes {
  implicit val booleanColumnType = BooleanColumnType
  implicit val intColumnType = IntColumnType
  implicit val longColumnType = LongColumnType
  implicit val doubleColumnType = DoubleColumnType
  implicit val bigDecimalColumnType = BigDecimalColumnType
  implicit val stringColumnType = StringColumnType
  implicit val dateTimeColumnType = DateTimeColumnType

  implicit def optionType[A](implicit base: BaseColumnType[A]): OptionColumnType[A] =
    OptionColumnType[A](base)
}

/**
 * Trait providing implicit constructors for `ColumnTypeEquivalences`, used to type-check
 * the arguments to sqlest expressions produced by traits in the `sqlest.ast.syntax` package.
 */
trait ColumnTypeEquivalences {
  implicit def baseBaseNumericEquivalence[A: NumericBaseColumnType, B: NumericBaseColumnType](implicit left: ColumnType[A], right: ColumnType[B], leftNumeric: Numeric[A], rightNumeric: Numeric[B]) =
    ColumnTypeEquivalence(left, right)

  implicit def baseOptionNumericEquivalence[A: NumericBaseColumnType, B: NumericBaseColumnType](implicit left: ColumnType[A], right: ColumnType[Option[B]], leftNumeric: Numeric[A], rightNumeric: Numeric[B]) =
    ColumnTypeEquivalence(left, right)

  implicit def optionBaseNumericEquivalence[A: NumericBaseColumnType, B: NumericBaseColumnType](implicit left: ColumnType[Option[A]], right: ColumnType[B], leftNumeric: Numeric[A], rightNumeric: Numeric[B]) =
    ColumnTypeEquivalence(left, right)

  implicit def optionOptionNumericEquivalence[A: NumericBaseColumnType, B: NumericBaseColumnType](implicit left: ColumnType[Option[A]], right: ColumnType[Option[B]], leftNumeric: Numeric[A], rightNumeric: Numeric[B]) =
    ColumnTypeEquivalence(left, right)

  implicit def baseBaseEquivalence[A: NonNumericBaseColumnType](implicit left: ColumnType[A], right: ColumnType[A]) =
    ColumnTypeEquivalence(left, right)

  implicit def baseOptionEquivalence[A: NonNumericBaseColumnType](implicit left: ColumnType[A], right: ColumnType[Option[A]]) =
    ColumnTypeEquivalence(left, right)

  implicit def optionBaseEquivalence[A: NonNumericBaseColumnType](implicit left: ColumnType[Option[A]], right: ColumnType[A]) =
    ColumnTypeEquivalence(left, right)

  implicit def optionOptionEquivalence[A: NonNumericBaseColumnType](implicit left: ColumnType[Option[A]], right: ColumnType[Option[A]]) =
    ColumnTypeEquivalence(left, right)

  implicit def mappedColumnTypeEquivalence[A, B](implicit left: MappedColumnType[A, B], right: MappedColumnType[A, B]) =
    ColumnTypeEquivalence(left, right)
}

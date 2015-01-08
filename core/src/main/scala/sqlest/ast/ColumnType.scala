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
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Object representing a mapping between a Scala data type and an underlying SQL data type.
 * Column types are broadly divided into three categories:
 *
 *  - `ColumnTypes with BaseColumnType` represent Scala data types that can be directly mapped to SQL types;
 *  - `OptionColumnTypes` represent Scala `Option` types that are mapped to nullable SQL types;
 *  - `MappedColumnTypes` represent arbitrary mappings between Scala types and SQL types.
 */
sealed trait ColumnType[A]

/** Marker trait representing a basic (non-optional) column type. */
sealed trait BaseColumnType
sealed trait NumericColumnType extends BaseColumnType
sealed trait NonNumericColumnType extends BaseColumnType

case object IntColumnType extends ColumnType[Int] with NumericColumnType
case object LongColumnType extends ColumnType[Long] with NumericColumnType
case object DoubleColumnType extends ColumnType[Double] with NumericColumnType
case object BigDecimalColumnType extends ColumnType[BigDecimal] with NumericColumnType

case object BooleanColumnType extends ColumnType[Boolean] with NonNumericColumnType
case object StringColumnType extends ColumnType[String] with NonNumericColumnType
case object DateTimeColumnType extends ColumnType[DateTime] with NonNumericColumnType

/**
 * Object representing an nullable SQL column type that is mapped to an `Option` in Scala.
 *
 * For every `OptionColumnType` there is an underlying `BaseColumnType`.
 */
case class OptionColumnType[A](baseType: ColumnType[A]) extends ColumnType[Option[A]]

/**
 * Object representing a custom column type `A` with an underlying base type `B`.
 *
 * For every `MappedColumnType` there is an underlying `BaseColumnType`.
 */
trait MappedColumnType[A, B] extends ColumnType[A] {
  def baseType: ColumnType[B]
  def read(database: B): A
  def write(value: A): B

  def compose[C: ColumnType](inner: MappedColumnType[B, C]): MappedColumnType[A, C] = {
    MappedColumnType((db: C) => this.read(inner.read(db)), (v: A) => inner.write(this.write(v)))
  }
}

object MappedColumnType {
  /**
   * Convenience method for constructing a `MappedColumnType` from a pair of
   * bidirectional mapping functions and an implicitly provided `BaseColumnType`.
   */
  def apply[A, B: ColumnType](r: B => A, w: A => B) = new MappedColumnType[A, B] {
    val baseType = implicitly[ColumnType[B]]
    def read(database: B) = r(database)
    def write(value: A) = w(value)
  }
}

object ColumnType {
  implicit val booleanColumnType = BooleanColumnType
  implicit val intColumnType = IntColumnType
  implicit val longColumnType = LongColumnType
  implicit val doubleColumnType = DoubleColumnType
  implicit val bigDecimalColumnType = BigDecimalColumnType
  implicit val stringColumnType = StringColumnType
  implicit val dateTimeColumnType = DateTimeColumnType
  implicit def materialize[A, B]: MappedColumnType[A, B] = macro MaterializeColumnTypeMacro.materializeImpl[A, B]

  implicit def optionType[A](implicit base: ColumnType[A]): OptionColumnType[A] =
    OptionColumnType[A](base)

  implicit class OptionColumnTypeOps[A](left: ColumnType[A]) {
    def toOptionColumnType: OptionColumnType[A] = left match {
      case option: OptionColumnType[A] => option
      case base => OptionColumnType(base)
    }
  }
}

case class MaterializeColumnTypeMacro(c: Context) {
  import c.universe._

  def materializeImpl[A: c.WeakTypeTag, B: c.WeakTypeTag] = {
    val typeOfA = c.weakTypeOf[A]
    val companion = typeOfA.typeSymbol.companion
    val applyMethod = findMethod(companion.typeSignature, "apply", typeOfA)
    val unapplyMethod = findMethod(companion.typeSignature, "unapply", typeOfA)
    val typeOfB = applyMethod.paramLists.head.head.asTerm.typeSignature
    q"MappedColumnType[$typeOfA, $typeOfB]($companion.$applyMethod, $companion.$unapplyMethod(_).get)"
  }

  def findMethod(companionType: Type, name: String, typeOfA: Type) = {
    val applyMethods = companionType.member(TermName(name)) match {
      case method: MethodSymbol => List(method)
      case termSymbol: TermSymbol => termSymbol.alternatives.collect { case method: MethodSymbol => method }
      case _ => Nil
    }

    val singleParamApplyMethods = applyMethods.filter(_.paramLists.flatten.length == 1)

    if (singleParamApplyMethods.length == 1) singleParamApplyMethods.head
    else c.abort(c.enclosingPosition, s"No matching $name method found on $typeOfA")
  }
}

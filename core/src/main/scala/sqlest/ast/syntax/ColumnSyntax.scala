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

package sqlest.ast.syntax

import sqlest.ast._

trait ColumnSyntax {
  implicit def literalColumn[A](value: A)(implicit columnType: ColumnType[A]): Column[A] =
    LiteralColumn[A](value)

  implicit def literalColumn[A](value: Some[A])(implicit columnType: ColumnType.Aux[A, A]): Column[Option[A]] =
    LiteralColumn[Option[A]](value)

  implicit class LiteralColumnOps[A](left: A) {
    def column[B >: A](implicit columnType: ColumnType[B]) = LiteralColumn[B](left)
  }

  /**
   * This enrichment allows writing 1.constant or "abc".constant, which will directly embed
   * the constant value into the generated sql statement. Do not use this on user input as
   * you will enable SQL injection attacks
   */
  implicit class ConstantColumnOps[A](value: A) {
    def constant[B >: A](implicit columnType: ColumnType[B]) = ConstantColumn[B](value)
  }

  /**
   * This implicit allows the use of `TableColumn -> Column` in setters
   */
  implicit def columnSetterPair[A, B](pair: (TableColumn[A], Column[B]))(implicit equivalence: ColumnTypeEquivalence[A, B]) =
    Setter[A, B](pair._1, pair._2)

  /**
   * This implicit allows the use of `TableColumn -> Value` in setters,
   * as opposed to `TableColumn -> Column` as is actually required:
   */
  implicit def literalSetterPair[A, B](pair: (TableColumn[A], B))(implicit valueType: ColumnType[B], equivalence: ColumnTypeEquivalence[A, B]) =
    Setter[A, B](pair._1, pair._2.column)

  implicit class AliasColumnOps[A](left: Column[A]) {
    def as(alias: String) = left match {
      case AliasColumn(column, _) => AliasColumn[A](column, alias)(left.columnType)
      case _ => AliasColumn[A](left, alias)(left.columnType)
    }

    def as(tableAlias: String, alias: String) = left match {
      case AliasColumn(column, _) => AliasColumn[A](column, tableAlias + "_" + alias)(left.columnType)
      case _ => AliasColumn[A](left, tableAlias + "_" + alias)(left.columnType)
    }
  }

  /**
   * This implicit conversion allows using as a column: a select statement which selects a single column
   */
  implicit def SelectColumnOps[A](select: Select[AliasedColumn[A], _ <: Relation]) = {
    val column = select.cols
    AliasColumn(SelectColumn(select)(column.columnType), column.columnAlias)(column.columnType)
  }

  implicit class NullableColumnsOps[A](left: Column[A]) {
    def isNull = PostfixFunctionColumn[Boolean]("is null", left)
    def isNotNull = PostfixFunctionColumn[Boolean]("is not null", left)
  }

  implicit class AliasedOptionColumnsOps[A](left: AliasedColumn[A]) {
    def ? = left match {
      case column: TableColumn[_] => AliasColumn(column, left.columnAlias)(left.columnType.toOptionColumnType)
      case AliasColumn(column, columnAlias) => AliasColumn(column, columnAlias)(left.columnType.toOptionColumnType)
      case column: ReferenceColumn[A] => ReferenceColumn(left.columnAlias)(left.columnType.toOptionColumnType)
    }
  }

  implicit class ComparisonColumnOps[A](left: Column[A]) {
    implicit val leftType: ColumnType[A] = left.columnType

    def ===[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean]("=", mappedLeft, mappedRight)
    }

    def =!=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean]("<>", mappedLeft, mappedRight)
    }

    def >[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean](">", mappedLeft, mappedRight)
    }

    def <[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean]("<", mappedLeft, mappedRight)
    }

    def >=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean](">=", mappedLeft, mappedRight)
    }

    def <=[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val (mappedLeft, mappedRight) = mapLiterals(left, right, equivalence)
      InfixFunctionColumn[Boolean]("<=", mappedLeft, mappedRight)
    }

    def between[B, C](lower: Column[B], upper: Column[C])(implicit lowerEquivalence: ColumnTypeEquivalence[A, B], upperEquivalence: ColumnTypeEquivalence[A, C]) = {
      val (mappedLeftLower, mappedLower) = mapLiterals(left, lower, lowerEquivalence)
      val (mappedLeftUpper, mappedUpper) = mapLiterals(left, upper, upperEquivalence)
      if (mappedLeftLower == mappedLeftUpper)
        DoubleInfixFunctionColumn[Boolean]("between", "and", left, mappedLower, mappedUpper)
      else
        throw new AssertionError("Cannot use between with different MappedColumns for lower and upper")
    }

    def in[B](values: Column[B]*)(implicit equivalence: ColumnTypeEquivalence[A, B]) = {
      val mappedValues = values.map(value => mapLiterals(left, value, equivalence)._2)
      InfixFunctionColumn[Boolean]("in", left, ScalarFunctionColumn("", mappedValues))
    }

    def in[B](values: List[B])(implicit rightType: ColumnType[B], equivalence: ColumnTypeEquivalence[A, B]): Column[Boolean] =
      in(values.map(_.constant): _*)

    // TODO - Is it possible to make this a macro in order to report illegal comparisons at compile time?
    private def mapLiterals(left: Column[_], right: Column[_], equivalence: ColumnTypeEquivalence[_, _]): (Column[_], Column[_]) = {

      def mapLiteralColumn(mappedColumnType: MappedColumnType[_, _], isOption: Boolean, column: Column[_]): Column[_] =
        column match {
          case LiteralColumn(value) => LiteralColumn(mappedValue(mappedColumnType, isOption, column, value))(mappedColumnType.baseColumnType.asInstanceOf[ColumnType[Any]])
          case ConstantColumn(value) => ConstantColumn(mappedValue(mappedColumnType, isOption, column, value))(mappedColumnType.baseColumnType.asInstanceOf[ColumnType[Any]])
          case _ => throw new AssertionError(s"Cannot compare MappedColumn $mappedColumnType and non mapped column $column")
        }

      def mappedValue[A](mappedColumnType: MappedColumnType[A, _], isOption: Boolean, column: Column[_], value: Any): Any =
        (isOption, column.columnType) match {
          case (true, _: BaseColumnType[_]) => mappedColumnType.write(Some(value).asInstanceOf[A])
          case (false, _: OptionColumnType[_, _]) => mappedColumnType.write(value.asInstanceOf[Option[_]].get.asInstanceOf[A])
          case _ => mappedColumnType.write(value.asInstanceOf[A])
        }

      (left.columnType, right.columnType) match {
        case (leftColumnType, rightColumnType) if leftColumnType == rightColumnType => (left, right)
        case (leftColumnType: MappedColumnType[_, _], rightColumnType: MappedColumnType[_, _]) if leftColumnType.baseColumnType == rightColumnType.baseColumnType => (left, right)
        case (leftColumnType: MappedColumnType[_, _], rightColumnType: MappedColumnType[_, _]) => throw new AssertionError(s"Cannot compare 2 different MappedColumns: $leftColumnType and $rightColumnType")
        case (leftColumnType: MappedColumnType[_, _], _) => (left, mapLiteralColumn(leftColumnType, equivalence.leftOption, right))
        case (_, rightColumnType: MappedColumnType[_, _]) => (mapLiteralColumn(rightColumnType, equivalence.rightOption, left), right)
        case (_, _) => (left, right)
      }
    }
  }

  implicit class BooleanColumnOps[A](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[Boolean, A]) {
    def unary_! = PrefixFunctionColumn[Boolean]("not", left)
    def &&[B](right: Column[B])(implicit equivalenceB: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("and", left, right)
    def ||[B](right: Column[B])(implicit equivalenceB: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("or", left, right)
  }

  implicit class IntColumnOps[A: ColumnType](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[Int, A]) {
    def +[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("+", left, right)
    def -[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("-", left, right)
    def *[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Int]("*", left, right)
    def /[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Double]("/", left, right)
  }

  implicit class StringColumnOps[A](left: Column[A])(implicit equivalence: ColumnTypeEquivalence[String, A]) {
    def ++[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[String]("||", left, right)
    def like[B](right: Column[B])(implicit equivalence: ColumnTypeEquivalence[A, B]) = InfixFunctionColumn[Boolean]("like", left, right)
  }

}

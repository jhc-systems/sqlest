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

import scala.util.Try
import sqlest.ast._
import sqlest.ast.syntax._

class UntypedColumnHelpers extends ColumnSyntax {
  def mappedArgument[A](arg: String, columnType: ColumnType[A])(implicit U: UntypedReads[A]) = columnType match {
    case mappedColumnType: MappedColumnType[A, _] => for {
      r <- U.reads(arg)
      _ <- Try(mappedColumnType.write(r.asInstanceOf[A])).toOption
    } yield r

    case _ => U.reads(arg)
  }

  def infixExpression[A](op: String, left: Column[A], right: String, columnType: ColumnType[A])(implicit U: UntypedReads[A]): Option[InfixFunctionColumn[Boolean]] =
    mappedArgument(right, columnType).map(r => InfixFunctionColumn[Boolean](op, left, LiteralColumn(r)(columnType.asInstanceOf[ColumnType[A]])))

  def likeExpression[A](left: Column[A], right: String, columnType: ColumnType[A], formatArgument: String => String)(implicit U: UntypedReads[A]): Option[InfixFunctionColumn[Boolean]] =
    mappedArgument(formatArgument(right), columnType).map(r => InfixFunctionColumn[Boolean]("like", left, LiteralColumn(r)(columnType.asInstanceOf[ColumnType[A]])))

  def likeEncode(str: String) =
    str.replaceAll("([%_^])", "^$1")
}

trait UntypedColumnSyntax {
  implicit class UntypedColumnOps[A](left: Column[A])(implicit U: UntypedReads[A]) {
    val helpers = new UntypedColumnHelpers

    def untypedEq(right: String) = helpers.infixExpression("=", left, right, left.columnType)
    def untypedNe(right: String) = helpers.infixExpression("<>", left, right, left.columnType)
    def untypedGt(right: String) = helpers.infixExpression(">", left, right, left.columnType)
    def untypedLt(right: String) = helpers.infixExpression("<", left, right, left.columnType)
    def untypedGte(right: String) = helpers.infixExpression(">=", left, right, left.columnType)
    def untypedLte(right: String) = helpers.infixExpression("<=", left, right, left.columnType)

    def untypedIn(right: List[String]) = {
      val mappedValues = right.map(value => helpers.mappedArgument(value, left.columnType))
      if (mappedValues.forall(_.isDefined)) {
        val inColumns = mappedValues.flatten.map(value => LiteralColumn(value)(left.columnType.asInstanceOf[ColumnType[A]]))
        Some(InfixFunctionColumn[Boolean]("in", left, ScalarFunctionColumn("", inColumns)(left.columnType)))
      } else
        None
    }

    def untypedContains(right: String): Option[InfixFunctionColumn[Boolean]] =
      helpers.likeExpression(left, right, left.columnType, str => s"%${helpers.likeEncode(right)}%")

    def untypedStartsWith(right: String): Option[InfixFunctionColumn[Boolean]] =
      helpers.likeExpression(left, right, left.columnType, str => s"${helpers.likeEncode(right)}%")

    def untypedEndsWith(right: String): Option[InfixFunctionColumn[Boolean]] =
      helpers.likeExpression(left, right, left.columnType, str => s"%${helpers.likeEncode(right)}")

    def untypedIsNull = Some(PostfixFunctionColumn[Boolean]("is null", left))
  }
}

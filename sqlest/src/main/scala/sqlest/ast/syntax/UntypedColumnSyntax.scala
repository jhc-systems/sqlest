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

import org.joda.time.DateTime
import scala.reflect.runtime.{ universe => ru }
import scala.util.Try
import sqlest.ast._
import sqlest.ast.syntax._
import sqlest.util.Iso8601

class UntypedColumnHelpers extends ColumnSyntax {
  def stringArgument(arg: String) = Some(arg)
  def intArgument(arg: String) = Try(arg.toInt).toOption
  def longArgument(arg: String) = Try(arg.toLong).toOption
  def doubleArgument(arg: String) = Try(arg.toDouble).toOption
  def booleanArgument(arg: String) = arg.trim.toLowerCase match {
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }
  def bigDecimalArgument(arg: String) = Try(BigDecimal(arg)).toOption
  def dateTimeArgument(arg: String) = Iso8601.unapply(arg)
  def byteArrayArgument(arg: String) = Try(javax.xml.bind.DatatypeConverter.parseHexBinary(arg)).toOption
  def mappedArgument[A](arg: String, columnType: ColumnType[A]): Option[A] = (columnType match {
    case IntColumnType => intArgument(arg)
    case LongColumnType => longArgument(arg)
    case DoubleColumnType => doubleArgument(arg)
    case BigDecimalColumnType => bigDecimalArgument(arg)
    case BooleanColumnType => booleanArgument(arg)
    case StringColumnType => stringArgument(arg)
    case DateTimeColumnType => dateTimeArgument(arg)
    case ByteArrayColumnType => byteArrayArgument(arg)
    case _ => sys.error(s"Untyped operators are not implemented for non-standard mapped types: $columnType")
  }).asInstanceOf[Option[A]]

  def infixExpression[A](op: String, left: Column[A], right: String, columnType: ColumnType[_]): Option[InfixFunctionColumn[Boolean]] = columnType match {
    case IntColumnType => intArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case LongColumnType => longArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case DoubleColumnType => doubleArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case BigDecimalColumnType => bigDecimalArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case BooleanColumnType => booleanArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case StringColumnType => stringArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case DateTimeColumnType => dateTimeArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case ByteArrayColumnType => byteArrayArgument(right).map(right => InfixFunctionColumn[Boolean](op, left, right))
    case optionColumnType: OptionColumnType[_, _] => infixExpression(op, left, right, optionColumnType.baseColumnType)
    case mappedColumnType: MappedColumnType[_, _] =>
      mappedArgument(right, mappedColumnType).map { right =>
        val mappedRight = mappedColumnType.write(right)
        InfixFunctionColumn[Boolean](op, left, LiteralColumn(mappedRight)(mappedColumnType.baseColumnType))
      }
  }

  def likeExpression(left: Column[_], right: String, columnType: ColumnType[_], formatArgument: String => String): Option[InfixFunctionColumn[Boolean]] = columnType match {
    case StringColumnType => stringArgument(right).map(right => InfixFunctionColumn[Boolean]("like", left, formatArgument(right)))
    case optionColumnType: OptionColumnType[_, _] => likeExpression(left, right, optionColumnType.baseColumnType, formatArgument)
    case _ => None
  }

  def likeEncode(str: String) =
    str.replaceAll("([%_^])", "^$1")

}

trait UntypedColumnSyntax {
  implicit class UntypedColumnOps(left: Column[_]) {
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
        val inColumns = mappedValues.flatten.map(value => LiteralColumn(value)(left.columnType.asInstanceOf[ColumnType[Any]]))
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

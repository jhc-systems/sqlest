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
 * A column, column literal, or column expression.
 *
 * This is a more general concept than it may first seem.
 * For example, in the SQL:
 *
 * ```
 * SELECT a, b, c FROM t WHERE a == 1
 * ```
 *
 * all of the following would be represented as instances of `Column`:
 * `a`, `b`, `c`, `1`, and `a == 1`.
 */
sealed trait Column[+A] {
  def columnType: ColumnType[A]
}

/** Literal column values, e.g. `1`, `'foo'`, and so on. */
case class LiteralColumn[A](value: A)(implicit val columnType: ColumnType[A]) extends Column[A]

/** Constant column values, e.g. `1`, `'foo'`, and so on. These are embedded directly in sql statements so beware of sql injection */
case class ConstantColumn[A](value: A)(implicit val columnType: ColumnType[A]) extends Column[A]

/** Unary prefix functions and operators, e.g. `not a`. */
case class PrefixFunctionColumn[A](name: String, parameter: Column[_])(implicit val columnType: ColumnType[A]) extends Column[A]

/** Binary functions and operators, e.g. `1 + 2`, `sum(1, 2)`. */
case class InfixFunctionColumn[A](name: String, parameter1: Column[_], parameter2: Column[_])(implicit val columnType: ColumnType[A]) extends Column[A]

/** Unary postfix functions and operators, e.g. `a is null`. */
case class PostfixFunctionColumn[A](name: String, parameter: Column[_])(implicit val columnType: ColumnType[A]) extends Column[A]

/** Binary prefix, infix operators, e.g. `a between 1 and 2` */
case class DoubleInfixFunctionColumn[A](infix1: String, infix2: String, parameter1: Column[_], parameter2: Column[_], parameter3: Column[_])(implicit val columnType: ColumnType[A]) extends Column[A]

/**
 * A ScalarFunctionColumn should not be created by the user directly.
 * Instead it will be returned as a result of applying a ScalarFunctionN (where N is a number)
 * to a set of columns. See ScalarFunctions for the implementations of ScalarFunctionN
 */
case class ScalarFunctionColumn[A](name: String, parameters: Seq[Column[_]])(implicit val columnType: ColumnType[A]) extends Column[A]

/**
 * A column that has an alias associated with it.
 *
 * The columns selected by a relation must all have aliases.
 * This is how we address them in the JDBC ResultSet:
 *
 * ```
 * SELECT a as a_alias, b as b_alias, c as c_alias FROM ...
 * ```
 */
sealed trait AliasedColumn[A] extends Column[A] {
  def columnAlias: String
}

/** Columns from tables. */
case class TableColumn[A](tableAlias: String, columnName: String)(implicit val columnType: ColumnType[A]) extends AliasedColumn[A] {
  def columnAlias = tableAlias + "_" + columnName
}

/** Alias for a column expression, e.g. `expression as alias` */
case class AliasColumn[A](column: Column[_], columnAlias: String)(implicit val columnType: ColumnType[A]) extends AliasedColumn[A]

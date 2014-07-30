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

trait AggregateFunctionSyntax {
  def count[A](column: Column[A] = AliasColumn[String](ConstantColumn("*"), "*")) = ScalarFunctionColumn[Int]("count", Seq(column)).as("count")
  def sum[A: Numeric](column: Column[A]) = AliasColumn((ScalarFunctionColumn[A]("sum", Seq(column))(column.columnType)), "sum")(column.columnType)
  def min[A: Numeric](column: Column[A]) = AliasColumn((ScalarFunctionColumn[A]("min", Seq(column))(column.columnType)), "min")(column.columnType)
  def max[A: Numeric](column: Column[A]) = AliasColumn((ScalarFunctionColumn[A]("max", Seq(column))(column.columnType)), "max")(column.columnType)
  def avg[A: Numeric](column: Column[A]) = AliasColumn[Double](ScalarFunctionColumn[Double]("avg", Seq(column)), "avg")
}

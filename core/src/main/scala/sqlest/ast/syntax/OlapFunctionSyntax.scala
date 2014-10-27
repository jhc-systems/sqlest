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

trait OlapFunction

trait OlapFunctionSyntax extends WindowFunctions {
  def denseRank() = new ScalarFunctionColumn[Int]("denserank", Seq()) with OlapFunction
  def rank() = new ScalarFunctionColumn[Int]("rank", Seq()) with OlapFunction
  def rowNumber() = new ScalarFunctionColumn[Int]("rownumber", Seq()) with OlapFunction

  implicit class OlapFunctionOps[A](olapFunction: ScalarFunctionColumn[A] with OlapFunction) {
    def over(windowFunction: WindowFunctionColumn = WindowFunctionColumn(Nil, Nil)) = AliasColumn(InfixFunctionColumn("", olapFunction, new ScalarFunctionColumn[Int]("over", Seq(windowFunction)))(olapFunction.columnType), olapFunction.name)(olapFunction.columnType)
  }
}

trait WindowFunctions {
  def partitionBy(columns: Column[_]*) = new WindowFunctionBuilder {
    def orderBy(orders: Order*) = WindowFunctionColumn(columns, orders)
  }

  def orderBy(orders: Order*) = WindowFunctionColumn(Nil, orders)
}

trait WindowFunctionBuilder {
  def orderBy(orders: Order*): WindowFunctionColumn
}
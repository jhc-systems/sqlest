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

trait ScalarFunctionSyntax extends ScalarFunctions {
  val trim = ScalarFunction1[String, String]("trim")
  val substring = ScalarFunction3[String, Int, Int, String]("substring")

  def coalesce[A](columns: Column[A]*) = ScalarFunctionColumn[A]("coalesce", columns)(columns.head.columnType)

  def connectByRoot[A](column: Column[A]) = ScalarFunctionColumn[A]("connect_by_root", Seq(column))(column.columnType)
  def prior[A](column: Column[A]) = ScalarFunctionColumn[A]("prior", Seq(column))(column.columnType)
}

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

trait CaseSyntax {
  def `case`() = CaseBuilder
  def decode() = CaseBuilder

  def `case`[A](whens: When[A]*)(implicit columnType: ColumnType[A]) =
    CaseWhenColumn(whens.toList)

  def decode[A](whens: When[A]*)(implicit columnType: ColumnType[A]) =
    CaseWhenColumn(whens.toList)

  def when[A](condition: Column[Boolean], result: Column[A]) =
    When(condition, result)

  implicit class CaseWhenColumnOps[A](caseWhen: CaseWhenColumn[A]) {
    implicit val columnType = caseWhen.columnType

    def when(condition: Column[Boolean], result: Column[A]) =
      CaseWhenColumn(caseWhen.whens :+ When(condition, result))

    def `else`(result: Column[A]) =
      CaseWhenElseColumn(caseWhen.whens, result)

    def otherwise(result: Column[A]) =
      CaseWhenElseColumn(caseWhen.whens, result)
  }

  implicit def defaultAliasCaseColumn[A](caseColumn: CaseColumn[A]) = AliasColumn(caseColumn, "case")(caseColumn.columnType)
}

object CaseBuilder {
  def when[A](condition: Column[Boolean], result: Column[A])(implicit columnType: ColumnType[A]) =
    CaseWhenColumn[A](List(When(condition, result)))
}
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

  def `case`[A](column: Column[A]) = CaseColumnBuilder(column)
  def decode[A](column: Column[A]) = CaseColumnBuilder(column)

  def `case`[A](whens: When[A]*) =
    CaseWhenColumn(whens.toList)(whens.head.result.columnType)

  def decode[A](whens: When[A]*) =
    CaseWhenColumn(whens.toList)(whens.head.result.columnType)

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

  implicit class CaseColumnColumnOps[A, B](caseColumn: CaseColumnColumn[A, B]) {
    implicit val columnType = caseColumn.columnType

    def when[C](value: Column[C], result: Column[A])(implicit equivalence: ColumnTypeEquivalence[B, C]) =
      CaseColumnColumn[A, B](caseColumn.column, caseColumn.mappings :+ (value, result))

    def `else`(result: Column[A]) =
      CaseColumnElseColumn[A, B](caseColumn.column, caseColumn.mappings, result)

    def otherwise(result: Column[A]) =
      CaseColumnElseColumn[A, B](caseColumn.column, caseColumn.mappings, result)
  }

  implicit def defaultAliasCaseColumn[A](caseColumn: CaseColumn[A]) = AliasColumn(caseColumn, "case")(caseColumn.columnType)
}

object CaseBuilder {
  def when[A](condition: Column[Boolean], result: Column[A]) =
    CaseWhenColumn[A](List(When(condition, result)))(result.columnType)
}

case class CaseColumnBuilder[B](column: Column[B]) {
  def when[C, A](value: Column[C], result: Column[A])(implicit equivalence: ColumnTypeEquivalence[B, C]) =
    CaseColumnColumn[A, B](column, List((value, result)))(result.columnType)
}

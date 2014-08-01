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

package sqlest.executor

import shapeless._
import shapeless.ops.hlist._
import shapeless.UnaryTCConstraint._
import sqlest.ast._
import sqlest.extractor._

trait ExecutorSyntax {
  implicit class SelectExecutorOps[AliasedColumns <: HList: *->*[AliasedColumn]#λ](select: Select[AliasedColumns])(implicit database: Database) {
    def fetchOne[A](extractor: Extractor[A]): Option[extractor.SingleResult] =
      database.executeSelect(select)(row => extractor.extractOne(row))

    def fetchAll[A](extractor: Extractor[A]): List[extractor.SingleResult] =
      database.executeSelect(select)(row => extractor.extractAll(row))

    def fetchOne[EH <: HList, SingleResult](implicit comapped: Comapped.Aux[AliasedColumns, AliasedColumn, EH], tupler: Tupler.Aux[EH, SingleResult]): Option[SingleResult] =
      fetchOne(HListExtractor(select.what))

    def fetchAll[EH <: HList, SingleResult](implicit comapped: Comapped.Aux[AliasedColumns, AliasedColumn, EH], tupler: Tupler.Aux[EH, SingleResult]): List[SingleResult] =
      fetchAll(HListExtractor(select.what))

  }

  implicit class InsertExecutorOps(insert: Insert) {
    def execute(implicit database: Database): Int = database.executeInsert(insert)
  }

  implicit class UpdateExecutorOps(update: Update) {
    def execute(implicit database: Database): Int = database.executeUpdate(update)
  }

  implicit class DeleteExecutorOps(delete: Delete) {
    def execute(implicit database: Database): Int = database.executeDelete(delete)
  }

  implicit class BatchExecutorOps(batchCommands: Seq[Command]) {
    def executeBatch(implicit database: Database): List[Int] = database.executeBatch(batchCommands)
  }
}

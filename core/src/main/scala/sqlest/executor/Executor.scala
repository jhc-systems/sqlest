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

import java.sql.ResultSet
import shapeless._
import shapeless.ops.hlist._
import shapeless.poly._
import sqlest.ast._
import sqlest.extractor._

private object columnExtractor extends (AliasedColumn ~> Extractor) {
  def apply[T](column: AliasedColumn[T]) = ColumnExtractor(column)
}

trait ExecutorSyntax {
  implicit class SelectExecutorOps[AliasedColumns <: HList](select: Select[AliasedColumns])(implicit database: Database) {
    def fetchOne[A](extractor: Extractor[A]): Option[extractor.SingleResult] =
      database.executeSelect(select)(row => extractor.extractOne(row))

    def fetchAll[A](extractor: Extractor[A]): List[extractor.SingleResult] =
      database.executeSelect(select)(row => extractor.extractAll(row))

    def fetchOne[IdHList <: HList, Extractors <: HList, Emitter <: HList, ResultSets <: HList, SingleResult](implicit mapper: Mapper.Aux[columnExtractor.type, AliasedColumns, Extractors],
      extractorComapped: Comapped.Aux[Extractors, Extractor, IdHList],
      isHCons: IsHCons[IdHList],
      columnComapped: Comapped.Aux[AliasedColumns, AliasedColumn, IdHList],
      toList: ToList[Extractors, Extractor[_]],
      initializerMapper: Mapper.Aux[extractorEmitter.type, Extractors, Emitter],
      constMapper: ConstMapper.Aux[ResultSet, Extractors, ResultSets],
      zipper: ZipApply.Aux[Emitter, ResultSets, IdHList],
      tupler: Tupler.Aux[IdHList, SingleResult]): Option[SingleResult] = {

      fetchOne(HListExtractor(select.what.map(columnExtractor))).map(_.tupled)
    }

    def fetchAll[IdHList <: HList, Extractors <: HList, Emitter <: HList, ResultSets <: HList, SingleResult](implicit mapper: Mapper.Aux[columnExtractor.type, AliasedColumns, Extractors],
      extractorComapped: Comapped.Aux[Extractors, Extractor, IdHList],
      isHCons: IsHCons[IdHList],
      columnComapped: Comapped.Aux[AliasedColumns, AliasedColumn, IdHList],
      toList: ToList[Extractors, Extractor[_]],
      initializerMapper: Mapper.Aux[extractorEmitter.type, Extractors, Emitter],
      constMapper: ConstMapper.Aux[ResultSet, Extractors, ResultSets],
      zipper: ZipApply.Aux[Emitter, ResultSets, IdHList],
      tupler: Tupler.Aux[IdHList, SingleResult]): List[SingleResult] = {

      fetchAll(HListExtractor(select.what.map(columnExtractor))).map(_.tupled)
    }
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

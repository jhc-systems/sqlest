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

import sqlest.ast._
import sqlest.extractor._

trait ExecutorSyntax {
  implicit class SelectExecutorOps[A](select: Select[A, _ <: Relation])(implicit database: Database) {
    def fetchHead[SingleResult](implicit extractable: Extractable.Aux[A, SingleResult]): SingleResult =
      fetchHeadOption.getOrElse(throw new NoSuchElementException("fetchHead when no results returned"))

    def fetchHeadOption[SingleResult](implicit extractable: Extractable.Aux[A, SingleResult]): Option[SingleResult] =
      database.executeSelect(select)(row => extractable.extractor(select.cols).extractHeadOption(row))

    def fetchAll[SingleResult](implicit extractable: Extractable.Aux[A, SingleResult]): List[SingleResult] =
      database.executeSelect(select)(row => extractable.extractor(select.cols).extractAll(row))

    def extractHead(extractor: Extractor[_]): extractor.SingleResult =
      extractHeadOption(extractor).getOrElse(throw new NoSuchElementException("extractHead when no results returned"))

    def extractHeadOption(extractor: Extractor[_]): Option[extractor.SingleResult] =
      database.executeSelect(select.what(extractorColumns(extractor)))(row => extractor.extractHeadOption(row))

    def extractAll(extractor: Extractor[_]): List[extractor.SingleResult] =
      database.executeSelect(select.what(extractorColumns(extractor)))(row => extractor.extractAll(row))

    private def extractorColumns(extractor: Extractor[_]): List[AliasedColumn[_]] = {
      extractor match {
        case ConstantExtractor(_) => Nil
        case ColumnExtractor(column) => List(column)
        case _: CellExtractor[_] => Nil
        case productExtractor: ProductExtractor[_] => productExtractor.innerExtractors.flatMap(extractorColumns)
        case MappedExtractor(innerExtractor, _) => extractorColumns(innerExtractor)
        case OptionExtractor(innerExtractor) => extractorColumns(innerExtractor)
        case SeqExtractor(extractors) => extractors.flatMap(extractorColumns).toList
        case ListMultiExtractor(innerExtractor) => extractorColumns(innerExtractor)
        case GroupedMultiExtractor(innerExtractor, groupByExtractor) => extractorColumns(innerExtractor) ++ extractorColumns(groupByExtractor)
      }
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

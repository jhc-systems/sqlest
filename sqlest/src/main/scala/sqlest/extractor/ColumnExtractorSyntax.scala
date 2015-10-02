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

package sqlest.extractor

import sqlest.ast._

trait ColumnExtractorSyntax {
  def extractColumnByName[A: ColumnType](name: String) = AliasColumn[A](null, name)

  implicit class ColumnExtractorOps(extractor: Extractor[java.sql.ResultSet, _]) {
    def findColumn(path: String): Option[AliasedColumn[_]] = extractor.findCellExtractor(path) match {
      case Some(aliasedColumn: AliasedColumn[_]) => Some(aliasedColumn)
      case _ => None
    }

    def columns: List[AliasedColumn[_]] = {
      extractor match {
        case ConstantExtractor(_) => Nil
        case column: AliasedColumn[_] => List(column)
        case _: CellExtractor[_, _] => Nil
        case productExtractor: ProductExtractor[_, _] => productExtractor.innerExtractors.flatMap(_.columns)
        case MappedExtractor(innerExtractor, _, _) => innerExtractor.columns
        case choiceExtractor: ChoiceExtractor[_, _, _] => choiceExtractor.inner.columns
        case OptionExtractor(innerExtractor) => innerExtractor.columns
        case NonOptionExtractor(innerExtractor) => innerExtractor.columns
        case SeqExtractor(extractors) => extractors.flatMap(_.columns).toList
        case ListMultiRowExtractor(innerExtractor) => innerExtractor.columns
        case GroupedExtractor(innerExtractor, groupByExtractor) => innerExtractor.columns ++ groupByExtractor.columns
        case exception: ExceptionExtractor[_, _] => Nil
      }
    }
  }
}

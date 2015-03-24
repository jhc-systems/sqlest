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

import java.sql.ResultSet
import sqlest.ast._

trait ColumnExtractorSetters {
  implicit class ExtractorSetters[A](extractor: Extractor[ResultSet, A]) {
    def settersFor(value: A): List[Setter[_, _]] = {
      extractor match {
        case tableColumn: TableColumn[a] => List(Setter(tableColumn, LiteralColumn(value.asInstanceOf[a])(tableColumn.columnType))(ColumnTypeEquivalence(false, false)))

        case MappedExtractor(innerExtractor, _, Some(unapplyMethod)) =>
          val values = unapplyMethod(value).get
          innerExtractor.settersFor(values)

        case productExtractor: ProductExtractor[ResultSet, _] =>
          val values = value match {
            case _: Seq[_] => List(value) // Will only happen if an outer MappedExtractor unapplyMethod returns a single value
            case value: Product => value.productIterator.toList
          }

          productExtractor.innerExtractors.zip(values).flatMap {
            case (extractor: Extractor[ResultSet, a], value) => extractor.settersFor(value.asInstanceOf[a])
          }

        case optionExtractor: OptionExtractor[ResultSet, a] =>
          value.asInstanceOf[Option[a]] match {
            case Some(value) => optionExtractor.inner.settersFor(value)
            case None => Nil
          }

        case seqExtractor: SeqExtractor[ResultSet, a] =>
          val values = value.asInstanceOf[Seq[a]]
          seqExtractor.extractors.zip(values).flatMap {
            case (extractor, value) => extractor.settersFor(value)
          }.toList

        case ConstantExtractor(_) => Nil
        case _: MappedExtractor[ResultSet, _, _] => throw new Exception(s"Cannot use settersFor with a MappedExtractor without an unapplyMethod - $extractor")
        case _: ListMultiRowExtractor[ResultSet, _] => throw new Exception("Cannot use settersFor with a ListMultiRowExtractor")
        case _: GroupedExtractor[ResultSet, _, _] => throw new Exception("Cannot use settersFor with a GroupedExtractor")
        case _ => throw new Exception(s"Cannot use settersFor with $extractor")
      }
    }
  }
}

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

import org.joda.time.DateTime
import sqlest.ast._

/**
 * Function capable of building an extractor of type `Result` from a value of type `A`.
 * `A` is typically a column or another extractor.
 *
 * We use this in `Extractors` to allow us to build large extractors from
 * mixed tuples of columns and extractors.
 */
trait ExtractorBuilder[-A] {
  type Result
  def apply(preExtractor: A): Extractor[Result]
}

trait ExtractorSyntax extends Extractors {
  implicit def identityExtractorBuilder[A] = new ExtractorBuilder[Extractor[A]] {
    type Result = A
    def apply(extractor: Extractor[A]): Extractor[A] = extractor
  }

  implicit def columnExtractorBuilder[A] = new ExtractorBuilder[AliasedColumn[A]] {
    type Result = A
    def apply(column: AliasedColumn[A]) = extractColumn(column)
  }

  def extractColumnByName[A: ColumnType](name: String)(implicit builder: ExtractorBuilder[AliasedColumn[A]]): CellExtractor[A] =
    builder(AliasColumn[A](null, name)).asInstanceOf[CellExtractor[A]]
}

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

case class ColumnExtractor[A](column: AliasedColumn[A]) extends CellExtractor[A] {
  def read(row: Row) =
    column.columnType match {
      case baseColumnType: BaseColumnType[A] => readRow(row, baseColumnType)
      case optionColumnType: OptionColumnType[_, _] => optionColumnType.read(readRow(row, optionColumnType.baseColumnType)).asInstanceOf[Option[A]]
      case mappedColumnType: MappedColumnType[_, _] => mappedColumnType.read(readRow(row, mappedColumnType.baseColumnType))
    }

  def readRow[B](row: Row, columnType: BaseColumnType[B]): Option[B] = columnType match {
    case IntColumnType => row.cellValue[Int](column.columnAlias)
    case LongColumnType => row.cellValue[Long](column.columnAlias)
    case DoubleColumnType => row.cellValue[Double](column.columnAlias)
    case BigDecimalColumnType => row.cellValue[BigDecimal](column.columnAlias)
    case BooleanColumnType => row.cellValue[Boolean](column.columnAlias)
    case StringColumnType => row.cellValue[String](column.columnAlias)
    case DateTimeColumnType => row.cellValue[DateTime](column.columnAlias)
  }
}

trait ColumnExtractors {
  implicit def extractColumn[A](column: AliasedColumn[A]) = ColumnExtractor(column)

  def extractColumnByName[A: ColumnType](name: String) =
    extractColumn(AliasColumn[A](null, name))

  implicit def columnExtractorBuilder[A] = new ExtractorBuilder[AliasedColumn[A], A] {
    def apply(column: AliasedColumn[A]) = ColumnExtractor(column)
  }
}

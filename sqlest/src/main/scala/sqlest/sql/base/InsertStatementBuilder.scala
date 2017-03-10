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

package sqlest.sql.base

import sqlest.ast._

trait InsertStatementBuilder extends BaseStatementBuilder {
  selectStatementBuilder: SelectStatementBuilder =>

  def insertSql(insert: Insert, indent: Int): String = insert match {
    case insert: InsertValues =>
      val insertColumns = insertColumnsSql(insert.columns)
      val insertValues = insertValuesSql(insert.columns)
      withLineBreaks(insertColumns, indent + TabWidth)(s"insert ${insertIntoSql(insert.into)} (", ", ", ")") +
        onNewLine(withLineBreaks(insertValues, indent + 7)(s"values (", ", ", ")"), indent)
    case InsertFromSelect(into, columns, select) =>
      val insertColumns = insertColumnsSql(columns)
      val insertValues = insertValuesSql(columns)
      withLineBreaks(insertColumns, indent + TabWidth)(s"insert ${insertIntoSql(into)} (", ", ", ")") +
        onNewLine(selectStatementBuilder.selectSql(select, indent), indent)
  }

  def insertIntoSql(into: Table): String =
    s"into ${identifierSql(into.tableName)}"

  def insertColumnsSql(columns: Seq[TableColumn[_]]): Seq[String] =
    columns.map(column => identifierSql(column.columnName))

  def insertValuesSql(columns: Seq[TableColumn[_]]): Seq[String] = columns.map(_ => "?")

  // -------------------------------------------------

  def insertArgs(insert: Insert): List[List[LiteralColumn[_]]] = insert match {
    case InsertValues(_, setterLists) => setterLists.map(_.toList.flatMap(setterArgs(_))).toList
    case InsertFromSelect(_, _, select) => List(selectStatementBuilder.selectArgs(select))
  }
}

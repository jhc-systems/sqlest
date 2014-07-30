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

package sqlest.sql

import sqlest.ast._

trait InsertStatementBuilder extends BaseStatementBuilder {
  selectStatementBuilder: SelectStatementBuilder =>

  def insertSql(insert: Insert): String = insert match {
    case insert: InsertValues =>
      s"insert ${insertIntoSql(insert.into)} ${insertColumnsSql(insert.columns)} ${insertValuesSql(insert.columns)}"
    case InsertFromSelect(into, columns, select) =>
      s"insert ${insertIntoSql(into)} ${insertColumnsSql(columns)} ${selectStatementBuilder.selectSql(select)}"
  }

  def insertIntoSql(into: Table): String =
    s"into ${identifierSql(into.tableName)}"

  def insertColumnsSql(columns: Seq[TableColumn[_]]): String =
    columns map (column => identifierSql(column.columnName)) mkString ("(", ", ", ")")

  def insertValuesSql(columns: Seq[TableColumn[_]]): String =
    columns.map(_ => "?") mkString ("values (", ", ", ")")

  // -------------------------------------------------

  def insertArgs(insert: Insert): List[LiteralColumn[_]] = insert match {
    case InsertValues(_, setterLists) => insertValuesArgs(setterLists)
    case InsertFromSelect(_, _, select) => selectStatementBuilder.selectArgs(select)
  }

  def insertValuesArgs(setterLists: Seq[Seq[Setter[_, _]]]): List[LiteralColumn[_]] =
    setterLists.flatten.toList.map(_.value) flatMap {
      case column: ConstantColumn[_] => List(LiteralColumn(column.value)(column.columnType))
      case column => columnArgs(column)
    }
}

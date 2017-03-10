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

trait UpdateStatementBuilder extends BaseStatementBuilder {
  def updateSql(update: Update, indent: Int): String = {
    Seq(
      updateTableSql(update.table),
      updateSetSql(update.set, indent)
    ) ++ Seq(
        updateWhereSql(update.where, indent)
      ).flatten mkString (NewLine + padding(indent))
  }

  def updateTableSql(table: Table): String =
    s"update ${identifierSql(table.tableName)}"

  def updateSetSql(setters: Seq[Setter[_, _]], indent: Int): String = {
    val setterSql = setters.map(setter => identifierSql(setter.column.columnName) + " = " + columnSql(setter.value, indent))
    withLineBreaks(setterSql, indent + TabWidth)("set ", ", ", "")
  }

  def updateWhereSql(where: Option[Column[Boolean]], indent: Int): Option[String] =
    where map (where => s"where ${columnSql(where, indent)}")

  // -------------------------------------------------

  def updateArgs(update: Update): List[LiteralColumn[_]] =
    updateSetArgs(update.set) ++
      updateWhereArgs(update.where)

  def updateSetArgs(setters: Seq[Setter[_, _]]): List[LiteralColumn[_]] =
    setters.toList.flatMap(setterArgs(_))

  def updateWhereArgs(where: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    where map columnArgs getOrElse Nil
}

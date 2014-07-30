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

trait UpdateStatementBuilder extends BaseStatementBuilder {
  def updateSql(update: Update): String = {
    Seq(
      updateTableSql(update.table),
      updateSetSql(update.set)
    ) ++ Seq(
        updateWhereSql(update.where)
      ).flatten mkString ("", " ", "")
  }

  def updateTableSql(table: Table): String =
    s"update ${identifierSql(table.tableName)}"

  def updateSetSql(setters: Seq[Setter[_, _]]): String =
    "set " + setters.map(setter => identifierSql(setter.column.columnName) + " = " + columnSql(setter.value)).mkString(", ")

  def updateWhereSql(where: Option[Column[Boolean]]): Option[String] =
    where map (where => s"where ${columnSql(where)}")

  // -------------------------------------------------

  def updateArgs(update: Update): List[LiteralColumn[_]] =
    updateSetArgs(update.set) ++
      updateWhereArgs(update.where)

  def updateSetArgs(setters: Seq[Setter[_, _]]): List[LiteralColumn[_]] =
    setters.toList flatMap (setter => columnArgs(setter.value))

  def updateWhereArgs(where: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    where map columnArgs getOrElse Nil
}

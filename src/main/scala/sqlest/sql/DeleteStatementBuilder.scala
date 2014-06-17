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

trait DeleteStatementBuilder extends BaseStatementBuilder {
  def deleteSql(delete: Delete): String = {
    Seq(
      "delete",
      deleteFromSql(delete.from)
    ) ++ Seq(
        deleteWhereSql(delete.where)
      ).flatten mkString ("", " ", "")
  }

  def deleteFromSql(from: Table): String =
    s"from ${identifierSql(from.tableName)}"

  def deleteWhereSql(where: Option[Column[Boolean]]): Option[String] =
    where map (where => s"where ${columnSql(where)}")

  // -------------------------------------------------

  def deleteArgs(delete: Delete): List[LiteralColumn[_]] =
    deleteWhereArgs(delete.where)

  def deleteWhereArgs(where: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    where map columnArgs getOrElse Nil
}

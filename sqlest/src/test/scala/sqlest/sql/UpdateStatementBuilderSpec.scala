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

import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.ast._

class UpdateStatementBuilderSpec extends BaseStatementBuilderSpec {
  implicit def statementBuilder = new base.StatementBuilder {}

  "update" should "produce the right sql" in {
    sql {
      update(TableOne)
        .set(
          TableOne.col1 -> "a",
          TableOne.col2 -> "b"
        )
        .where("c".column === "d".column && "e".column === "f".column)
    } should equal(
      s"""
       |update one
       |set col1 = ?, col2 = ?
       |where ((? = ?) and (? = ?))
       """.formatSql,
      List(List("a", "b", "c", "d", "e", "f"))
    )
  }

  it should "produce the right sql for an aliased table" in {
    val tableOneAliased = new TableOne(Some("one_alias"))
    sql {
      update(tableOneAliased)
        .set(
          tableOneAliased.col1 -> "a",
          tableOneAliased.col2 -> "b"
        )
        .where(tableOneAliased.col1 === "d".column && "e".column === "f".column)
    } should equal(
      s"""
         |update one as one_alias
         |set col1 = ?, col2 = ?
         |where ((one_alias.col1 = ?) and (? = ?))
       """.formatSql,
      List(List("a", "b", "d", "e", "f"))
    )
  }
}
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

trait UpdateStatementBuilderSpec extends BaseStatementBuilderSpec {
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
       """.trim.stripMargin.split(lineSeparator).mkString(" "),
      List("a", "b", "c", "d", "e", "f")
    )
  }
}
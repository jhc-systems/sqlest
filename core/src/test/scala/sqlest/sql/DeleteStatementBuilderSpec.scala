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

trait DeleteStatementBuilderSpec extends BaseStatementBuilderSpec {
  "delete" should "produce the right sql" in {
    sql {
      delete
        .from(TableOne)
        .where(1.column === 2 && 3.column =!= 4.column)
    } should equal(
      s"""
       |delete
       |from one
       |where ((? = ?) and (? <> ?))
       """.formatSql,
      List(1, 2, 3, 4)
    )
  }
}
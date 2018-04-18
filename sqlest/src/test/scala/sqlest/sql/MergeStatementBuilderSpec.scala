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

import sqlest._
import sqlest.ast._

class MergeStatementBuilderSpec extends BaseStatementBuilderSpec {
  implicit def statementBuilder = new base.StatementBuilder {}

  "merge" should "produce the right sql" in {

    val s = select.from(MyTable)
    val uSetters = Seq(Setter(MyTable.col1, MyTable.col2))
    val iSetters = Seq(Setter(MyTable.col1, 123))
    val c = MyTable.col1 === MyTable.col2
    val u: Update = update(MyTable).set(uSetters).where(c)
    val i: Insert = insert.into(MyTable).set(iSetters)
    val m = merge.into(MyTable).using(s).whenMatched(MatchedOp(Left(u))).whenNotMatched(NotMatchedOp(i)).on(c)
    sql(m) should equal(
      s"""
         |
       """.stripMargin
    )
  }

  "merge with more than one matched op" should "produce the right sql" in {

    val s = select.from(MyTable)
    val uSetters = Seq(Setter(MyTable.col1, MyTable.col2))
    val iSetters = Seq(Setter(MyTable.col1, 123))
    val c = MyTable.col1 === MyTable.col2
    val u1 = MatchedAndOp(
      op = Left(update(MyTable).set(uSetters).where(c)),
      and = MyTable.col1 === MyTable.col2
    )
    val u = MatchedOp(
      op = Left(update(MyTable).set(uSetters).where(c))
    )
    val d1 = MatchedAndOp(
      op = Right(""),
      and = MyTable.col1 === MyTable.col2
    )
    val d2 = MatchedAndOp(
      op = Right("garbage"),
      and = MyTable.col2 === MyTable.col1
    )
    val opList = List(u1, d1, d2)
    val i: Insert = insert.into(MyTable).set(iSetters)
    val m = merge
      .into(MyTable)
      .using(s)
      .whenMatched(u)
      .whenMatchedAnd(opList)
      .whenNotMatchedAnd(NotMatchedAndOp(i, c)).on(c)
    sql(m) should equal(
      s"""
         |
       """.stripMargin
    )
  }
}
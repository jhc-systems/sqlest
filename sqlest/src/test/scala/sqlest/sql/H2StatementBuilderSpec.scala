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

class H2StatementBuilderSpec extends BaseStatementBuilderSpec {
  val statementBuilder = H2StatementBuilder

  "where between" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1.between(123, 234))
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 between ? and ?)
       """.formatSql,
      List(List(123, 234))
    )
  }

  "where in" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 in (123, 234.constant, 345))
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 in (?, 234, ?))
       """.formatSql,
      List(List(123, 345))
    )
  }

  "where in list" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 in List(123, 234, 345))
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 in (123, 234, 345))
       """.formatSql,
      List(List())
    )
  }

  "select with limit only" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .limit(10)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = ?)
       |limit ?
       """.formatSql,
      List(List(123, 10))
    )
  }

  "select with offset only" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .offset(20)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |offset ?
       """.formatSql,
      List(List(20))
    )
  }

  "select with limit and offset" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .page(2, 10)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = ?)
       |limit ?
       |offset ?
       """.formatSql,
      List(List(123, 10, 20))
    )
  }

  "select with group by" should "produce the right sql" in {
    sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(MyTable.col1, MyTable.col2)
    } should equal(
      s"""
       |select sum(mytable.col1) as sum
       |from mytable
       |where (mytable.col1 > ?)
       |group by mytable.col1, mytable.col2
       """.formatSql,
      List(List(123))
    )
  }

  "select with group by & rollup" should "throw UnsupportedOperationException" in {
    an[UnsupportedOperationException] should be thrownBy sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(rollUp(MyTable.col1, MyTable.col2))
    }
  }

  "select with multiple placeholders" should "produce the right sql" in {
    sql {
      select(1.column as "a", sum(2) as "b", (3.column + 4.column) as "c")
        .from(TableOne innerJoin TableTwo on (5.column === 6 && 7.column =!= 8))
        .where(9.column === 10 && 11.column =!= 12.column)
        .orderBy(13.column.asc, 14.column.desc)
        .page(15, 16)
    } should equal(
      s"""
       |select ? as a, sum(?) as b, (? + ?) as c
       |from one inner join two on ((? = ?) and (? <> ?))
       |where ((? = ?) and (? <> ?))
       |order by ?, ? desc
       |limit ?
       |offset ?
       """.formatSql,
      List(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 15 * 16))
    )
  }

  "where clauses for option columns" should "produce the right sql" in {
    sql {
      select(TableThree.col3, TableThree.col4)
        .from(TableThree)
        .where(TableThree.col3 === "abc")
    } should equal(
      s"""
       |select three.col3 as three_col3, three.col4 as three_col4
       |from three
       |where (three.col3 = ?)
       """.formatSql,
      List(List("abc"))
    )
  }
}
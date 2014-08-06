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
import scala.language.reflectiveCalls
import sqlest._
import sqlest.ast._

trait SelectStatementBuilderSpec extends BaseStatementBuilderSpec {
  "empty query" should "render ok" in {
    sql {
      select.from(MyTable)
    } should equal(
      """select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2 from mytable""",
      Nil
    )
  }

  "select with aliased column function" should "produce the right sql" in {
    val func = sum(MyTable.col1) as "sum-function"

    sql {
      select(MyTable.col1, MyTable.col2, func)
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .orderBy(func.asc)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2, sum(mytable.col1) as sum-function
       |from mytable
       |where (mytable.col1 = ?)
       |order by sum-function
       """.formatSql,
      List(123)
    )
  }

  "query with simple where clause" should "render ok" in {
    sql {
      select
        .from(MyTable)
        .where(MyTable.col1 === 1)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = ?)
       """.formatSql,
      List(1)
    )
  }

  "query with compound where clause" should "render ok" in {
    sql {
      select
        .from(MyTable)
        .where(MyTable.col1 === 1 && MyTable.col2 =!= 2)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where ((mytable.col1 = ?) and (mytable.col2 <> ?))
       """.formatSql,
      List(1, 2)
    )
  }

  "select from a single table" should "produce the right sql" in {
    sql {
      select
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .orderBy(MyTable.col1.asc)
        .orderBy(MyTable.col1.asc)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = ?)
       |order by mytable.col1, mytable.col1
       """.formatSql,
      List(123)
    )
  }

  "select from a three table join" should "produce the right sql" in {
    sql {
      select
        .from(
          TableOne
            .innerJoin(TableTwo).on(TableOne.col2 === TableTwo.col2)
            .innerJoin(TableThree).on(TableTwo.col3 === TableThree.col3))
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, three.col3 as three_col3, three.col4 as three_col4
       |from ((one inner join two on (one.col2 = two.col2)) inner join three on (two.col3 = three.col3))
       """.formatSql,
      Nil
    )
  }

  "select with case statement" should "produce the right sql" in {
    sql {
      select(
        `case`()
          .when(MyTable.col1 === 1, 2)
          .when(MyTable.col2 === 2.constant, 3.constant)
          .`else`(5.constant))
        .from(MyTable)
    } should equal(
      s"""
       |select
       |  case
       |    when (mytable.col1 = ?) then ?
       |    when (mytable.col2 = 2) then 3
       |    else 5
       |  end as case
       |from mytable
       """.formatSql,
      List(1, 2)
    )

    sql {
      select(
        decode()
          .when(MyTable.col1 === 1, 2)
          .when(MyTable.col2 === 2.constant, 3.constant))
        .from(MyTable)
    } should equal(
      s"""
       |select
       |  case
       |    when (mytable.col1 = ?) then ?
       |    when (mytable.col2 = 2) then 3
       |  end as case
       |from mytable
       """.formatSql,
      List(1, 2)
    )

    sql {
      select(
        `case`(MyTable.col1)
          .when(1, 2)
          .when(2.constant, 3.constant))
        .from(MyTable)
    } should equal(
      s"""
       |select
       |  case mytable.col1
       |    when ? then ?
       |    when 2 then 3
       |  end as case
       |from mytable
       """.formatSql,
      List(1, 2)
    )

    sql {
      select(
        `case`(MyTable.col1)
          .when(1, 2)
          .when(2.constant, 3.constant)
          .`else`(4))
        .from(MyTable)
    } should equal(
      s"""
       |select
       |  case mytable.col1
       |    when ? then ?
       |    when 2 then 3
       |    else ?
       |  end as case
       |from mytable
       """.formatSql,
      List(1, 2, 4)
    )
  }

  "select with order by on OrderedColumnType" should "produce the right sql" in {
    sql {
      select(TableFour.orderedColumn)
        .from(TableFour)
        .orderBy(TableFour.orderedColumn.desc)
    } should equal(
      s"""
       |select four.orderedColumn as four_orderedColumn
       |from four
       |order by case four.orderedColumn when 'G' then 0 when 'S' then 1 when 'B' then 2 end desc
       """.formatSql,
      List()
    )
  }

  "select scalar function" should "produce the right sql" in {
    sql {
      select(TableThree.col3, TableThree.col4, testFunction(TableThree.col3, "abc").as("testFunction"))
        .from(TableThree)
    } should equal(
      s"""
       |select three.col3 as three_col3, three.col4 as three_col4, testFunction(three.col3, ?) as testFunction
       |from three
       """.formatSql,
      List("abc")
    )
  }

  "select connect by" should "produce the right sql" in {
    sql {
      select(TableOne.col1)
        .from(TableOne)
        .startWith(TableOne.col1 === "abc")
        .connectBy(prior(TableOne.col2) === TableOne.col2)
    } should equal(
      s"""
       |select one.col1 as one_col1
       |from one
       |start with (one.col1 = ?)
       |connect by (prior(one.col2) = one.col2)
       """.formatSql,
      List("abc")
    )
  }

}

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

class DB2StatementBuilderSpec extends BaseStatementBuilderSpec {
  val statementBuilder = DB2StatementBuilder

  // DB2-specific tests:

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
       |fetch first 10 rows only
       """.formatSql,
      List(List(123))
    )
  }

  "select with offset only" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .offset(20)
    } should equal(
      s"""
       |with subquery as (
       |  select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2, row_number() over () as rownum
       |  from mytable
       |  where (mytable.col1 = ?)
       |)
       |select mytable_col1, mytable_col2
       |from subquery
       |where rownum >= ?
       """.formatSql,
      List(List(123, 2 * 10 + 1))
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
       |with subquery as (
       |  select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2,
       |    row_number() over () as rownum
       |  from mytable
       |  where (mytable.col1 = ?)
       |)
       |select mytable_col1, mytable_col2
       |from subquery
       |where rownum between ? and ?
       """.formatSql,
      List(List(123, 2 * 10 + 1, 2 * 10 + 10))
    )
  }

  "select with limit and offset and order" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 123)
        .orderBy(MyTable.col1.asc)
        .page(2, 10)
    } should equal(
      s"""
       |with subquery as (
       |  select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2,
       |    row_number() over (order by mytable.col1) as rownum
       |  from mytable
       |  where (mytable.col1 = ?)
       |)
       |select mytable_col1, mytable_col2
       |from subquery
       |where rownum between ? and ?
       """.formatSql,
      List(List(123, 2 * 10 + 1, 2 * 10 + 10))
    )
  }

  "select with multiple placeholders" should "produce the right sql" in {
    sql {
      select(1.constant as "a", sum(2) as "b", (3.constant + 4.column) as "c")
        .from(TableOne innerJoin TableTwo on (5.column === 6 && 7.column =!= 8))
        .where(9.column === 10 && 11.column =!= 12.column)
        .orderBy(13.column.asc, 14.column.desc)
        .page(15, 16)
    } should equal(
      s"""
       |with subquery as (
       |  select 1 as a, sum(cast(? as integer)) as b, (3 + ?) as c,
       |    row_number() over (order by ?, ? desc) as rownum
       |  from one inner join two on ((? = ?) and (? <> ?))
       |  where ((? = ?) and (? <> ?))
       |)
       |select a, b, c
       |from subquery
       |where rownum between ? and ?
       """.formatSql,
      List(List(2, 4, 13, 14, 5, 6, 7, 8, 9, 10, 11, 12, 15 * 16 + 1, 15 * 16 + 16))
    )
  }

  "select table function" should "produce the right sql" in {
    sql {
      select(TableThree.col3, TableThree.col4, TestTableFunction.col5, TestTableFunction.col6)
        .from(TableThree.crossJoin(TestTableFunction(TableThree.col3, "abc")))
    } should equal(
      s"""
       |select three.col3 as three_col3, three.col4 as three_col4, testTableFunction.col5 as testTableFunction_col5, testTableFunction.col6 as testTableFunction_col6
       |from three cross join table(testTableFunction(three.col3, cast(? as varchar(256)))) as testTableFunction
       """.formatSql,
      List(List("abc"))
    )
  }

  "select table select function" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3)
        .from(TableOne)
        .leftJoin(tableFunction(select(TableTwo.col2, TableTwo.col3).from(TableTwo).where(TableTwo.col2 === "123")).as("testTableFunctionFromSelect"))
        .on(TableOne.col2 === TableTwo.col2)
    } should equal(
      s"""
         |select one.col1 as one_col1, one.col2 as one_col2, two_col2, two_col3
         |from one left join table(
         |  select two.col2 as two_col2, two.col3 as two_col3
         |  from two where (two.col2 = ?)
         |) as testTableFunctionFromSelect
         |on (one.col2 = two_col2)
       """.formatSql,
      List(List("123"))
    )
  }

  "select with group by rollup" should "produce the right sql" in {
    sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(rollUp(MyTable.col1, MyTable.col2))
    } should equal(
      s"""
       |select sum(mytable.col1) as sum
       |from mytable
       |where (mytable.col1 > ?)
       |group by rollup(mytable.col1, mytable.col2)
       """.formatSql,
      List(List(123))
    )
  }

  "select with group by column, tuple & function" should "produce the right sql" in {
    sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(MyTable.col1, cube((MyTable.col1, MyTable.col2), MyTable.col2))
    } should equal(
      s"""
       |select sum(mytable.col1) as sum
       |from mytable
       |where (mytable.col1 > ?)
       |group by mytable.col1, cube((mytable.col1, mytable.col2), mytable.col2)
       """.formatSql,
      List(List(123))
    )
  }

  "select with nested group functions" should "produce the right sql" in {
    sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(groupingSets(MyTable.col1, cube((MyTable.col1, MyTable.col2), MyTable.col2), rollUp(MyTable.col2)), MyTable.col2)
    } should equal(
      s"""
       |select sum(mytable.col1) as sum
       |from mytable
       |where (mytable.col1 > ?)
       |group by grouping sets(mytable.col1, cube((mytable.col1, mytable.col2), mytable.col2), rollup(mytable.col2)), mytable.col2
       """.formatSql,
      List(List(123))
    )
  }

  "select with grouping sets & empty tuple" should "produce the right sql" in {
    sql {
      select(sum(MyTable.col1).as("sum"))
        .from(MyTable)
        .where(MyTable.col1 > 123)
        .groupBy(groupingSets(MyTable.col1, ()))
    } should equal(
      s"""
       |select sum(mytable.col1) as sum
       |from mytable
       |where (mytable.col1 > ?)
       |group by grouping sets(mytable.col1, ())
       """.formatSql,
      List(List(123))
    )
  }

  "select scalar function" should "produce the right sql" in {
    sql {
      select(TableThree.col3, TableThree.col4, testFunction(TableThree.col3, "abc").as("testFunction"))
        .from(TableThree)
    } should equal(
      s"""
       |select three.col3 as three_col3, three.col4 as three_col4, testFunction(three.col3, cast(? as varchar(256))) as testFunction
       |from three
       """.formatSql,
      List(List("abc"))
    )
  }

  "boolean columns" should "produce the right sql" in {
    sql {
      select(MyTable.col1)
        .from(MyTable)
        .where(true)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1
       |from mytable
       |where (? = ?)
       """.formatSql,
      List(List(0, 0))
    )

    sql {
      select(MyTable.col1)
        .from(MyTable)
        .where(false)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1
       |from mytable
       |where (? <> ?)
       """.formatSql,
      List(List(0, 0))
    )

    sql {
      select(MyTable.col1)
        .from(MyTable)
        .where(true.constant)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1
       |from mytable
       |where (0 = 0)
       """.formatSql,
      List(Nil)
    )

    sql {
      select(MyTable.col1)
        .from(MyTable)
        .where(false.constant)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1
       |from mytable
       |where (0 <> 0)
       """.formatSql,
      List(Nil)
    )

  }

  "inserting or updating unmapped boolean columns" should "throw an exception on DB2" in {
    intercept[AssertionError] {
      sql {
        insert
          .into(TableFive)
          .columns(TableFive.col1, TableFive.col2)
          .values(TableFive.col1 -> "a", TableFive.col2 -> true)
          .values(TableFive.col1 -> "b", TableFive.col2 -> false)
      }
    }

    intercept[AssertionError] {
      sql {
        update(TableFive)
          .set(TableFive.col1 -> "a", TableFive.col2 -> true)
          .updateAll
      }
    }
  }

  "exception joins" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3, TableThree.col3, TableThree.col4)
        .from(TableOne)
        .leftExceptionJoin(TableTwo).on(TableOne.col2 === TableTwo.col2)
        .rightExceptionJoin(TableThree).on(TableTwo.col3 === TableThree.col3)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, three.col3 as three_col3, three.col4 as three_col4
       |from one left exception join two on (one.col2 = two.col2) right exception join three on (two.col3 = three.col3)
       """.formatSql,
      List(Nil)
    )
  }

}

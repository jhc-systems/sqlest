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

class SelectStatementBuilderSpec extends BaseStatementBuilderSpec {
  implicit def statementBuilder = new base.StatementBuilder {}

  "empty query" should "render ok" in {
    sql {
      select(MyTable.col1, MyTable.col2.?)
        .from(MyTable)
    } should equal(
      """select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2 from mytable""",
      List(Nil)
    )
  }

  "query with simple where clause" should "render ok" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 1)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = ?)
       """.formatSql,
      List(List(1))
    )
  }

  "query with compound where clause" should "render ok" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === 1 && MyTable.col2 =!= 2)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where ((mytable.col1 = ?) and (mytable.col2 <> ?))
       """.formatSql,
      List(List(1, 2))
    )
  }

  "select from a single table" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
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
      List(List(123))
    )
  }

  "select from a three table join" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3, TableThree.col3, TableThree.col4)
        .from(TableOne)
        .innerJoin(TableTwo).on(TableOne.col2 === TableTwo.col2)
        .innerJoin(TableThree).on(TableTwo.col3 === TableThree.col3)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, three.col3 as three_col3, three.col4 as three_col4
       |from one inner join two on (one.col2 = two.col2) inner join three on (two.col3 = three.col3)
       """.formatSql,
      List(Nil)
    )
  }

  "select from a auto join" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3)
        .from(TableOne)
        .autoJoin(TableTwo)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3
       |from one inner join two on (one.col2 = two.col2)
       """.formatSql,
      List(Nil)
    )
  }

  it should "allow auto joins to a joined table" in {
    val TableTwoo = TableTwo.as("twoo")
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3, TableTwoo.col2, TableTwoo.col3)
        .from(TableOne)
        .innerJoin(TableTwo).on(TableOne.col2 === TableTwo.col2)
        .autoJoin(TableTwoo)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, twoo.col2 as twoo_col2, twoo.col3 as twoo_col3
       |from one inner join two on (one.col2 = two.col2) inner join two as twoo on (one.col2 = twoo.col2)
       """.formatSql,
      List(Nil)
    )

    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3, TableTwoo.col2, TableTwoo.col3)
        .from(TableTwo)
        .innerJoin(TableOne).on(TableOne.col2 === TableTwo.col2)
        .autoJoin(TableTwoo)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, twoo.col2 as twoo_col2, twoo.col3 as twoo_col3
       |from two inner join one on (one.col2 = two.col2) inner join two as twoo on (one.col2 = twoo.col2)
       """.formatSql,
      List(Nil)
    )
  }

  it should "allow many auto joins to the same table" in {
    val TableTwoo = TableTwo.as("twoo")
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3, TableTwoo.col2, TableTwoo.col3)
        .from(TableOne)
        .autoJoin(TableTwo)
        .autoJoin(TableTwoo)
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3, twoo.col2 as twoo_col2, twoo.col3 as twoo_col3
       |from one inner join two on (one.col2 = two.col2) inner join two as twoo on (one.col2 = twoo.col2)
       """.formatSql,
      List(Nil)
    )
  }

  it should "work with table functions" in {
    val NewTestTableFunction = TestTableFunction.as("newtesttablefunction")
    sql {
      select(TableOne.col1, TableOne.col2, TableTwo.col2, TableTwo.col3)
        .from(TableOne)
        .autoLeftJoin(TableTwo)
        .autoRightJoin(NewTestTableFunction(TableOne.col1, "abc".constant))
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2, two.col2 as two_col2, two.col3 as two_col3
       |from one left join two on (one.col2 = two.col2) right join testTableFunction(one.col1, 'abc') as newtesttablefunction on (one.col1 = newtesttablefunction.col6)
       """.formatSql,
      List(Nil)
    )
  }

  "select with aggregate function" should "produce the right sql" in {
    sql {
      select(count(MyTable.col1), min(TableOne.col2), count(distinct(TableOne.col1)))
        .from(MyTable.crossJoin(TableOne))
        .where(MyTable.col1 === 123)
    } should equal(
      s"""
       |select count(mytable.col1) as count, min(one.col2) as min, count(distinct(one.col1)) as count
       |from mytable cross join one
       |where (mytable.col1 = ?)
       """.formatSql,
      List(List(123))
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
       |order by sum(mytable.col1)
       """.formatSql,
      List(List(123))
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
      List(List(1, 2))
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
      List(List(1, 2))
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
      List(List(1, 2))
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
      List(List(1, 2, 4))
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
      List(List())
    )
  }

  "select having" should "produce the right sql" in {
    sql {
      select(count())
        .from(MyTable)
        .groupBy(MyTable.col2)
        .having(sum(MyTable.col1) >= 5.constant)
    } should equal(
      s"""
       |select count('*') as count
       |from mytable
       |group by mytable.col2
       |having (sum(mytable.col1) >= 5)
       """.formatSql,
      List(List())
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
      List(List("abc"))
    )
  }

  "select table function" should "produce the right sql" in {
    sql {
      select(TableThree.col3, TableThree.col4, TestTableFunction.col5, TestTableFunction.col6)
        .from(TableThree.crossJoin(TestTableFunction(TableThree.col3, "abc")))
    } should equal(
      s"""
       |select three.col3 as three_col3, three.col4 as three_col4, testTableFunction.col5 as testTableFunction_col5, testTableFunction.col6 as testTableFunction_col6
       |from three cross join testTableFunction(three.col3, ?) as testTableFunction
       """.formatSql,
      List(List("abc"))
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
      List(List("abc"))
    )
  }

  "scalar subquery" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(MyTable)
        .where(MyTable.col1 === select(MyTable.col1).from(MyTable))
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |from mytable
       |where (mytable.col1 = (select mytable.col1 as mytable_col1 from mytable))
       """.formatSql,
      List(Nil)
    )
  }

  "union" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2)
        .from(TableOne)
        .union(
          select(TableTwo.col2, TableTwo.col3)
            .from(TableTwo))
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2
       |from one
       |union
       |select two.col2 as two_col2, two.col3 as two_col3
       |from two
       """.formatSql,
      List(Nil)
    )
  }

  "unionAll" should "produce the right sql" in {
    sql {
      select(TableOne.col1, TableOne.col2)
        .from(TableOne)
        .unionAll(
          select(TableTwo.col2, TableTwo.col3)
            .from(TableTwo))
    } should equal(
      s"""
       |select one.col1 as one_col1, one.col2 as one_col2
       |from one
       |union all
       |select two.col2 as two_col2, two.col3 as two_col3
       |from two
       """.formatSql,
      List(Nil)
    )
  }

  "subselect" should "produce the right sql" in {
    sql {
      select(MyTable.col1, MyTable.col2)
        .from(
          select(MyTable.col1, MyTable.col2)
            .from(MyTable).as("subselect"))
    } should equal(
      s"""
       |select mytable_col1, mytable_col2
       |from
       |  (select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |   from mytable) as subselect
       """.formatSql,
      List(Nil)
    )

    sql {
      select(MyTable.col1, MyTable.col2)
        .from(
          select(MyTable.col1, MyTable.col2)
            .from(
              select(MyTable.col1, MyTable.col2)
                .from(MyTable).as("subselect2")).as("subselect1"))
    } should equal(
      s"""
       |select mytable_col1, mytable_col2
       |from
       |  (select mytable_col1, mytable_col2
       |   from
       |    (select mytable.col1 as mytable_col1, mytable.col2 as mytable_col2
       |     from mytable) as subselect2) as subselect1
       """.formatSql,
      List(Nil)
    )
  }

  "olap functions" should "produce the right sql" in {
    sql {
      select(rowNumber().over())
        .from(MyTable)
    } should equal(
      s"""
       |select (rownumber()  over()) as rownumber
       |from mytable
       """.formatSql,
      List(Nil)
    )

    sql {
      select(rowNumber().over(partitionBy(MyTable.col1).orderBy(MyTable.col2)))
        .from(MyTable)
    } should equal(
      s"""
       |select (rownumber()  over(partition by mytable.col1 order by mytable.col2)) as rownumber
       |from mytable
       """.formatSql,
      List(Nil)
    )

    sql {
      val rowNumberColumn = rowNumber().over(partitionBy(MyTable.col1).orderBy(MyTable.col2))
      select(MyTable.col1)
        .from(MyTable)
        .innerJoin(
          select(rowNumberColumn)
            .from(MyTable))
        .on(rowNumberColumn === 1.constant)
    } should equal(
      s"""
       |select mytable.col1 as mytable_col1
       |from mytable
       |inner join (select (rownumber()  over(partition by mytable.col1 order by mytable.col2)) as rownumber from mytable)
       |  on (rownumber = 1)
       """.formatSql,
      List(Nil)
    )
  }
}

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

    val selectOp = select.from(MyTable)
    val updateSetters = Seq(Setter(MyTable.col1, MyTable.col2))
    val insertSetters = Seq(Setter(MyTable.col1, 123))
    val columnBoolean = MyTable.col1 === MyTable.col2
    val updateOp = update(MyTable).set(updateSetters).where(columnBoolean)
    val insertOp = insert.into(MyTable).set(insertSetters)
    val m = merge.into((MyTable, "a"))
      .using((selectOp, "b"))
      .on(columnBoolean)
      .whenMatched(updateOp).and(None)
      .whenNotMatched(insertOp).and(None)
    sql(m) should equal(
      s"""merge into mytable as a
               |using (select  from mytable) as b
               | on (mytable.col1 = mytable.col2)
               |  when matched  then UPDATE set col1 = mytable.col2
               |  when not matched  then INSERT (col1) values (?)""".formatSql,
      List(List(123))
    )
  }

  "merge with more than one matched op" should "produce the right sql" in {

    val selectOp = select.from(MyTable)
    val updateSetters = Seq(Setter(MyTable.col1, MyTable.col2))
    val insertSetters = Seq(Setter(MyTable.col1, 123))
    val columnBoolean = MyTable.col1 === MyTable.col2
    val updateOpOne = update(MyTable).set(updateSetters).where(columnBoolean)
    val updateOpTwo = update(MyTable).set(updateSetters).where(columnBoolean)
    val deleteOp = delete
    val insertOp = insert.into(MyTable).set(insertSetters)
    val mergeOp = merge
      .into((MyTable, "a"))
      .using((selectOp, "b"))
      .on(columnBoolean)
      .whenMatched(updateOpOne).and(None)
      .whenMatched(updateOpTwo).and(Some(MyTable.col1 === MyTable.col2))
      .whenMatched(deleteOp).and(Some(MyTable.col1 === MyTable.col2))
      .whenMatched(deleteOp).and(Some(MyTable.col2 === MyTable.col1))
      .whenNotMatched(insertOp).and(Some(columnBoolean))
    sql(mergeOp) should equal(
      s"""merge into mytable as a
               | using (select  from mytable) as b on (mytable.col1 = mytable.col2)
               |  when matched  then UPDATE set col1 = mytable.col2
               |  when matched AND (mytable.col2 = mytable.col1) then DELETE
               |  when matched AND (mytable.col1 = mytable.col2) then DELETE
               |  when matched AND (mytable.col1 = mytable.col2) then UPDATE set col1 = mytable.col2
               |  when not matched AND (mytable.col1 = mytable.col2) then INSERT (col1) values (?)""".formatSql,
      List(List(123))
    )
  }

  "merge when matched DELETE" should "produce the right sql" in {
    val selectOp = select.from(MyTable)
    val condition = MyTable.col1 === MyTable.col2
    val deleteOp = delete
    val insertFromSelect = InsertFromSelect(
      into = MyTable,
      columns = Seq(MyTable.col1),
      select = select(MyTable.col1).from(MyTable)
    )
    val mergeOp = merge
      .into((MyTable, "a"))
      .using((selectOp, "b"))
      .on(condition)
      .whenMatched(deleteOp).and(None)
      .whenNotMatched(insertFromSelect).and(Some(MyTable.col1 === MyTable.col2))
    sql(mergeOp) should equal(
      s"""merge into mytable as a
               | using (select  from mytable) as b on (mytable.col1 = mytable.col2)
               |  when matched  then DELETE
               |   when not matched AND (mytable.col1 = mytable.col2) then INSERT (col1)
               |    select mytable.col1 as mytable_col1 from mytable""".formatSql,
      List()
    )
  }

  "merge when not matched only " should "produces the right sql" in {
    val selectOp = select.from(MyTable)
    val condition = MyTable.col1 === MyTable.col2
    val insertFromSelect = InsertFromSelect(
      into = MyTable,
      columns = Seq(MyTable.col1),
      select = select(MyTable.col1).from(MyTable)
    )
    val mergeOp = merge
      .into((MyTable, "a"))
      .using((selectOp, "b"))
      .on(condition)
      .whenNotMatched(insertFromSelect).and(None)
    sql(mergeOp) should equal(
      s"""merge into mytable as a
           | using (select  from mytable) as b on (mytable.col1 = mytable.col2)  when
           |  not matched  then INSERT (col1) select mytable.col1 as mytable_col1 from mytable""".formatSql,
      List(List())
    )
  }

  "merge when not matched and when not matched and only " should "produces the right sql" in {
    val selectOp = select.from(MyTable)
    val condition = MyTable.col1 === MyTable.col2
    val insertFromSelect = InsertFromSelect(
      into = MyTable,
      columns = Seq(MyTable.col1),
      select = select(MyTable.col1).from(MyTable)
    )
    val mergeOp = merge
      .into((MyTable, "a"))
      .using((selectOp, "b"))
      .on(condition)
      .whenNotMatched(insertFromSelect).and(None)
      .whenNotMatched(insertFromSelect).and(Some(MyTable.col1 === MyTable.col2))
    sql(mergeOp) should equal(
      s"""merge into mytable as a
         | using (select  from mytable) as b on (mytable.col1 = mytable.col2)  when
         |  not matched  then INSERT (col1) select mytable.col1 as mytable_col1 from mytable when
         |   not matched AND (mytable.col1 = mytable.col2) then INSERT (col1) select mytable.col1 as mytable_col1 from mytable""".formatSql,
      List(List())
    )
  }
}
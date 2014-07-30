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

package sqlest.ast

import org.scalatest._
import org.scalatest.matchers._
import sqlest._

class SelectSpec extends FlatSpec with Matchers {
  class MyTable(alias: Option[String]) extends Table("mytable", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[Int]("col2")
  }
  object MyTable extends MyTable(None)

  "simplest select possible" should "produce the right sql" in {
    val query = select.from(MyTable)

    query.columns should equal(List(MyTable.col1, MyTable.col2))
    query.from should equal(MyTable)
    query.where should equal(None)
    query.orderBy should equal(Nil)
    query.limit should equal(None)
    query.offset should equal(None)
  }

  "select with explicit column list" should "produce the right sql" in {
    val query = select(MyTable.col1).from(MyTable)

    query.columns should equal(List(MyTable.col1))
    query.from should equal(MyTable)
    query.where should equal(None)
    query.orderBy should equal(Nil)
    query.limit should equal(None)
    query.offset should equal(None)
  }

  "repeated calls to select.where()" should "append new filters" in {
    val query = select.from(MyTable).where(MyTable.col1 > 1).where(MyTable.col1 < 2)
    query.where should equal(Some(MyTable.col1 > 1 && MyTable.col1 < 2))
  }

  "repeated calls to select.orderBy()" should "append new orders" in {
    val query = select.from(MyTable)
    query.columns should equal(List(MyTable.col1, MyTable.col2))
  }

  "repeated calls to select.page()" should "override the old values" in {
    val query = select.from(MyTable).page(1, 10).page(2, 20)
    query.limit should equal(Some(20))
    query.offset should equal(Some(40))
  }
}
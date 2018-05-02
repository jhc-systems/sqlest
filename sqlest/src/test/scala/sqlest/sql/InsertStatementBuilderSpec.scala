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
import sqlest.ast.{ Setter, LiteralColumn }

class InsertStatementBuilderSpec extends BaseStatementBuilderSpec {
  implicit def statementBuilder = new base.StatementBuilder {}

  "insert" should "produce the right sql" in {
    sql {
      insert
        .into(TableOne)
        .set(
          TableOne.col1 -> "a",
          TableOne.col2 -> "b"
        )
    } should equal(
      s"""
       |insert
       |into one
       |(col1, col2)
       |values (?, ?)
       """.formatSql,
      List(List("a", "b"))
    )
  }

  "bulk insert" should "produce the right sql" in {
    sql {
      insert
        .into(TableOne)
        .bulkInsert(Seq(("a1", "b1"), ("a2", "b2"))) { (x: (String, String)) =>
          Seq(
            TableOne.col1 -> x._1,
            TableOne.col2 -> x._2)
        }
    } should equal(
      s"""
         |insert
         |into one
         |(col1, col2)
         |values (?, ?)
       """.formatSql,
      List(List("a1", "b1"), List("a2", "b2"))
    )
  }

  "insert" should "treat constant columns as literals" in {
    sql {
      insert
        .into(TableOne)
        .set(
          TableOne.col1 -> "a".constant,
          TableOne.col2 -> "b"
        )
    } should equal(
      s"""
       |insert
       |into one
       |(col1, col2)
       |values (?, ?)
       """.formatSql,
      List(List("a", "b"))
    )
  }

  "insert columns" should "produce the right sql" in {
    sql {
      insert
        .into(TableOne)
        .columns(TableOne.col1, TableOne.col2)
        .values(TableOne.col1 -> "a", TableOne.col2 -> "b")
        .values(TableOne.col1 -> "c", TableOne.col2 -> "d")
    } should equal(
      s"""
       |insert
       |into one
       |(col1, col2)
       |values (?, ?)
       """.formatSql,
      List(List("a", "b"), List("c", "d"))
    )
  }

  "boolean columns" should "produce the right sql" in {
    sql {
      insert
        .into(TableFive)
        .columns(TableFive.col1, TableFive.col2)
        .values(TableFive.col1 -> "a", TableFive.col2 -> true)
        .values(TableFive.col1 -> "b", TableFive.col2 -> false)
    } should equal(
      s"""
      |insert
      |into five
      |(col1, col2)
      |values (?, ?)
    """.formatSql,
      List(List("a", true), List("b", false))
    )
  }

  "insert with newRecord" should "produce the right sql" in {
    sql {
      insert
        .into(TableOne)
        .set(
          TableOne.col1 -> "a",
          TableOne.col2 -> "b"
        )
        .newRecord
        .set(
          TableOne.col1 -> "c",
          TableOne.col2 -> "d"
        )
    } should equal(
      s"""
       |insert
       |into one
       |(col1, col2)
       |values (?, ?)
       """.formatSql,
      List(List("a", "b"), List("c", "d"))
    )
  }

  "insert columns from select" should "produce the right sql" in {
    sql {
      insert
        .into(TableOne)
        .columns(TableOne.col1)
        .from(
          select(TableOne.col1, TableOne.col2).from(TableOne).where(TableOne.col2 === "abc")
        )
    } should equal(
      s"""
       |insert
       |into one
       |(col1)
       |select one.col1 as one_col1, one.col2 as one_col2 from one where (one.col2 = ?)
       """.formatSql,
      List(List("abc"))
    )
  }
}

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

package sqlest.executor

import scala.reflect.runtime.universe.WeakTypeTag
import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.extractor.TestResultSet

class ExecutorSpec extends FlatSpec with Matchers {
  import TestData._

  implicit def testDatabase = TestDatabase(testResultSet)

  val selectStatement = select(TableOne.col1, TableOne.col2).from(TableOne)
  val updateStatement = update(TableOne).set(TableOne.col1 -> 123).where(TableOne.col2 === "12")
  val insertStatement = insert.into(TableOne).set(TableOne.col1 -> 123)
  val deleteStatement = delete.from(TableOne).where(TableOne.col2 === "12")

  val extractor = extract[One](
    a = TableOne.col1,
    b = TableOne.col2
  )

  "a select statement" should "be executable with an extractor" in {
    selectStatement.extractAll(extractor)
  }

  it should "be able to return all results" in {
    val actualExtracted = select.from(TableOne).extractAll(extractor)

    actualExtracted should equal(Seq(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "be able to return an option of the first result" in {
    val actual = selectStatement.extractHeadOption(extractor)

    actual should equal(Some(One(1, "a")))
  }

  it should "be able to return the first result" in {
    val actual = selectStatement.extractHead(extractor)

    actual should equal(One(1, "a"))
  }

  it should "be able to return the types of the columns passed in" in {
    val head: Int = select(TableOne.col1).from(TableOne).fetchHead
    head should be(1)

    val headOption: Option[String] = select(TableOne.col2).from(TableOne).fetchHeadOption
    headOption should be(Some("a"))

    val all: List[(Int, String)] = select(TableOne.col1, TableOne.col2).from(TableOne).fetchAll
    all should equal(Seq(
      (1, "a"),
      (3, "c"),
      (-1, "e")
    ))
  }

  "a unioned select statement" should "be executable with an extractor with same number of columns to the select" in {
    val actualExtracted = select(TableOne.col1, TableOne.col2).from(TableOne)
      .union(select(TableOne.col1, TableOne.col2).from(TableOne))
      .extractAll(extractor)

    actualExtracted should equal(Seq(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "not be executable with an extractor with a different number of columns to the select" in {
    intercept[AssertionError] {
      select(TableOne.col1).from(TableOne)
        .union(select(TableOne.col1).from(TableOne))
        .extractAll(extractor)
    }
  }

  "an update" should "require a transaction to run" in {
    // Ensure the same database is used throughout this test
    implicit val testDatabase = TestDatabase(testResultSet)

    intercept[AssertionError] {
      updateStatement.execute
    }

    testDatabase.withTransaction {
      updateStatement.execute
    }
  }

  "an insert" should "require a transaction to run" in {
    // Ensure the same database is used throughout this test
    implicit val testDatabase = TestDatabase(testResultSet)

    intercept[AssertionError] {
      insertStatement.execute
    }

    testDatabase.withTransaction {
      insertStatement.execute
    }
  }

  "a delete" should "require a transaction to run" in {
    // Ensure the same database is used throughout this test
    implicit val testDatabase = TestDatabase(testResultSet)

    intercept[AssertionError] {
      deleteStatement.execute
    }

    testDatabase.withTransaction {
      deleteStatement.execute
    }
  }
}

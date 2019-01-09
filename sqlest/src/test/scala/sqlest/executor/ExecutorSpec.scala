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

import org.joda.time.LocalDate
import org.scalatest._
import org.scalatest.matchers._
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.util.Try
import sqlest._
import sqlest.ast.{ LiteralColumn, Setter }
import sqlest.extractor.TestResultSet

import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSpec extends FlatSpec with Matchers {
  import TestData._

  implicit def testDatabase = TestDatabase(testResultSet)

  val selectStatement = select(TableOne.col1, TableOne.col2).from(TableOne)
  val updateStatement = update(TableOne).set(TableOne.col1 -> 123).where(TableOne.col2 === "12")
  val insertStatement = insert.into(TableOne).set(TableOne.col1 -> 123)
  val optionInsertStatement = insert.into(TableThree).values(TableThree.col3 -> Option[Int](1), TableThree.col4 -> Option[String](null))
  val mappedOptionInsertStatement1 = {
    insert
      .into(TableSix)
      .columns(TableSix.trimmedString)
      .values(Setter(TableSix.trimmedString, LiteralColumn(Some(WrappedString("a")): Option[WrappedString])))
  }
  val mappedOptionInsertStatement2 = {
    insert
      .into(TableSix)
      .columns(TableSix.trimmedString)
      .values(Setter(TableSix.trimmedString, LiteralColumn(None: Option[WrappedString])))
  }
  val mappedOptionSelectStatement1 = {
    select(TableSix.zeroIsNoneLocalDate)
      .from(TableSix)
      .where(TableSix.zeroIsNoneLocalDate === Option.empty[LocalDate])
  }
  val mappedOptionSelectStatement2 = {
    select(TableSix.zeroIsNoneLocalDate)
      .from(TableSix)
      .where(TableSix.zeroIsNoneLocalDate === Some(new LocalDate(2015, 1, 1)))
  }
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

  "withConnection" should "close the connection on completion" in {
    val database = TestDatabase(testResultSet)
    database.withConnection { connection =>
      connection.createStatement.execute("values(0)")
    }
    database.lastConnection.get.closed shouldBe true
  }

  it should "close the connection when an exception is thrown" in {
    val database = TestDatabase(testResultSet)
    intercept[Exception] {
      database.withConnection { connection =>
        throw new Exception("Catastrophic failure")
      }
    }
    database.lastConnection.get.closed shouldBe true
  }

  "an update" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      updateStatement.execute
    }
  }

  it should "close the connection on completion" in {
    val database = TestDatabase(testResultSet)
    database.withTransaction { implicit transaction =>
      updateStatement.execute
    }
    database.lastConnection.get.closed shouldBe true
  }

  it should "commit the transaction on success" in {
    val database = TestDatabase(testResultSet)
    database.withTransaction { implicit transaction =>
      updateStatement.execute
    }
    database.lastConnection.get.committed shouldBe true
  }

  it should "roll back the transaction and close the connection on rollback" in {
    val database = TestDatabase(testResultSet)
    database.withTransaction { implicit transaction =>
      val result = updateStatement.execute
      transaction.rollback
      result
    }
    database.lastConnection.get.closed shouldBe true
    database.lastConnection.get.rolledBack shouldBe true
    database.lastConnection.get.committed shouldBe false
  }

  it should "roll back the transaction and close the connection when an exception is thrown" in {
    val database = TestDatabase(testResultSet)
    intercept[Exception] {
      database.withTransaction { implicit transaction =>
        throw new Exception("Catastrophic error")
      }
    }
    database.lastConnection.get.closed shouldBe true
    database.lastConnection.get.rolledBack shouldBe true
    database.lastConnection.get.committed shouldBe false
  }

  "an asynchronous update" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransactionAsync { implicit transaction =>
      Future(updateStatement.execute)
    }
  }

  it should "close the connection on completion" in {
    val database = TestDatabase(testResultSet)
    val transaction = database.withTransactionAsync { implicit transaction =>
      Future(updateStatement.execute)
    }
    val result = Try(Await.result(transaction, 20.seconds))
    result shouldBe 'success
    database.lastConnection.get.closed shouldBe true
  }

  it should "commit the transaction on success" in {
    val database = TestDatabase(testResultSet)
    val transaction = database.withTransactionAsync { implicit transaction =>
      Future(updateStatement.execute)
    }
    val result = Try(Await.result(transaction, 20.seconds))
    result shouldBe 'success
    database.lastConnection.get.committed shouldBe true
  }

  it should "roll back the transaction and close the connection on rollback" in {
    val database = TestDatabase(testResultSet)
    val transaction = database.withTransactionAsync { implicit transaction =>
      Future {
        val result = updateStatement.execute
        transaction.rollback
        result
      }
    }
    val result = Try(Await.result(transaction, 20.seconds))
    result shouldBe 'success
    database.lastConnection.get.closed shouldBe true
    database.lastConnection.get.rolledBack shouldBe true
    database.lastConnection.get.committed shouldBe false
  }

  it should "roll back the transaction and close the connection when an exception is thrown" in {
    val database = TestDatabase(testResultSet)
    val transaction = database.withTransactionAsync { implicit transaction =>
      throw new Exception("Catastrophic error")
    }
    val result = Try(Await.result(transaction, 20.seconds))
    result shouldBe 'failure
    database.lastConnection.get.closed shouldBe true
    database.lastConnection.get.rolledBack shouldBe true
    database.lastConnection.get.committed shouldBe false
  }

  it should "roll back the transaction and close the connection when the Future fails" in {
    val database = TestDatabase(testResultSet)
    val transaction = database.withTransactionAsync { implicit transaction =>
      Future(throw new Exception("Catastrophic error"))
    }
    val result = Try(Await.result(transaction, 20.seconds))
    result shouldBe 'failure
    database.lastConnection.get.closed shouldBe true
    database.lastConnection.get.rolledBack shouldBe true
    database.lastConnection.get.committed shouldBe false
  }

  "an insert" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      insertStatement.execute
    }
  }

  "an asynchronous insert" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransactionAsync { implicit transaction =>
      Future(insertStatement.execute)
    }
  }

  "executeReturningKeys" should "return the generated String key" in {
    TestDatabase(testResultSet, Some(keyResultSet)).withTransaction { implicit transaction =>
      val keys: List[String] = insertStatement.executeReturningKeys[String]
      keys should equal(List[String]("34"))
    }
  }

  it should "return the generated Integer key" in {
    TestDatabase(testResultSet, Some(keyResultSet)).withTransaction { implicit transaction =>
      val keys: List[Int] = insertStatement.executeReturningKeys[Int]
      keys should equal(List[Int](46))
    }
  }

  "a delete" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      deleteStatement.execute
    }
  }

  "an asynchronous delete" should "require a transaction to run" in {
    TestDatabase(testResultSet).withTransactionAsync { implicit transaction =>
      Future(deleteStatement.execute)
    }
  }

  "an insert with optional values" should "execute correctly" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      optionInsertStatement.execute
    }
  }

  it should "generate raw SQL correctly" in {
    TestDatabase(testResultSet).statementBuilder.generateRawSql(optionInsertStatement) should equal(
      "insert into three (col3, col4) values (1, null)"
    )
  }

  it should "set parameters correctly" in {
    val testDatabase = TestDatabase(testResultSet)
    testDatabase.withTransaction { implicit transaction =>
      optionInsertStatement.execute
    }

    testDatabase.preparedStatement.get.sql shouldBe "insert into three (col3, col4) values (?, ?)"
    testDatabase.preparedStatement.get.parameters shouldBe Map(1 -> 1, 2 -> null)
  }

  "an insert with mapped optional values" should "execute correctly" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      mappedOptionInsertStatement1.execute
    }
  }

  it should "generate raw SQL correctly" in {
    TestDatabase(testResultSet).statementBuilder.generateRawSql(mappedOptionInsertStatement1) should equal(
      "insert into six (trimmedString) values ('a')"
    )

    testDatabase.statementBuilder.generateRawSql(mappedOptionInsertStatement2) should equal(
      "insert into six (trimmedString) values ('')"
    )

    testDatabase.statementBuilder.generateRawSql(mappedOptionSelectStatement1) should equal(
      "select six.zeroIsNoneDateTime as six_zeroIsNoneDateTime from six where (six.zeroIsNoneDateTime = 0)"
    )

    testDatabase.statementBuilder.generateRawSql(mappedOptionSelectStatement2) should equal(
      "select six.zeroIsNoneDateTime as six_zeroIsNoneDateTime from six where (six.zeroIsNoneDateTime = 20150101)"
    )
  }

  it should "set parameters correctly" in {
    val testDatabase = TestDatabase(testResultSet)

    testDatabase.withTransaction { implicit transaction =>
      mappedOptionInsertStatement1.execute
    }

    testDatabase.preparedStatement.get.sql shouldBe "insert into six (trimmedString) values (?)"
    testDatabase.preparedStatement.get.parameters shouldBe Map(1 -> "a")

    testDatabase.withTransaction { implicit transaction =>
      mappedOptionInsertStatement2.execute
    }

    testDatabase.preparedStatement.get.sql shouldBe "insert into six (trimmedString) values (?)"
    testDatabase.preparedStatement.get.parameters shouldBe Map(1 -> "")
  }

  it should "compile with both implicit database and transaction for a select" in {
    TestDatabase(testResultSet).withTransaction { implicit transaction =>
      selectStatement.fetchAll
      insertStatement.execute
    }
  }

  it should "return verbose exception messages when configured to do so" in {
    val database = TestDatabase(testResultSet, Some(keyResultSet), shouldThrow = true, verboseExceptionMessages = true)

    val selectException = intercept[SqlestException] {
      database.withSession { implicit session =>
        selectStatement.fetchAll
      }
    }

    assert(selectException.message.startsWith("Exception running sql"))
    selectException.cause shouldBe database.anException

    val insertException = intercept[SqlestException] {
      database.withTransaction { implicit transaction =>
        insertStatement.execute
      }
    }

    assert(insertException.message.startsWith("Exception running sql"))
    insertException.cause shouldBe database.anException

    val insertReturningKeysException = intercept[SqlestException] {
      database.withTransaction { implicit transaction =>
        insertStatement.executeReturningKeys[String]
      }
    }

    assert(insertReturningKeysException.message.startsWith("Exception running sql"))
    insertReturningKeysException.cause shouldBe database.anException

    val updateException = intercept[SqlestException] {
      database.withTransaction { implicit transaction =>
        updateStatement.execute
      }
    }

    assert(updateException.message.startsWith("Exception running sql"))
    updateException.cause shouldBe database.anException
  }

  it should "return the underlying exception otherwise" in {
    val database = TestDatabase(testResultSet, Some(keyResultSet), shouldThrow = true, verboseExceptionMessages = false)

    val selectException = intercept[Exception] {
      database.withSession { implicit session =>
        selectStatement.fetchAll
      }
    }

    selectException shouldBe database.anException

    val insertException = intercept[Exception] {
      database.withTransaction { implicit transaction =>
        insertStatement.execute
      }
    }

    insertException shouldBe database.anException

    val insertReturningKeysException = intercept[Exception] {
      database.withTransaction { implicit transaction =>
        insertStatement.executeReturningKeys[String]
      }
    }

    insertReturningKeysException shouldBe database.anException

    val updateException = intercept[Exception] {
      database.withTransaction { implicit transaction =>
        updateStatement.execute
      }
    }

    updateException shouldBe database.anException
  }

  it should "return verbose exception messages on prepare when configured to do so" in {
    val database = TestDatabase(testResultSet, Some(keyResultSet), shouldThrow = true, verboseExceptionMessages = true, throwExceptionOnPrepare = true)

    val selectException = intercept[SqlestException] {
      database.withSession { implicit session =>
        selectStatement.fetchAll
      }
    }

    assert(selectException.message.startsWith("Exception running sql"))
    selectException.cause shouldBe database.anException
  }

  it should "return the underlying exception on prepare otherwise " in {
    val database = TestDatabase(testResultSet, Some(keyResultSet), shouldThrow = true, verboseExceptionMessages = false, throwExceptionOnPrepare = true)

    val selectException = intercept[Exception] {
      database.withSession { implicit session =>
        selectStatement.fetchAll
      }
    }

    selectException shouldBe database.anException
  }
}

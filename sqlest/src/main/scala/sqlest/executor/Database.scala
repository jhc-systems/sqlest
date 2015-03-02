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

import sqlest.ast._
import sqlest.extractor._
import sqlest.sql.base._
import sqlest.util._

import java.sql.{ Connection, DriverManager, ResultSet, Statement, SQLException }
import javax.sql.DataSource
import java.util.Properties
import scala.util.DynamicVariable

object Database {
  def withDataSource(dataSource: DataSource, builder: StatementBuilder): Database = new Database {
    def getConnection: Connection = dataSource.getConnection
    val statementBuilder = builder
  }
}

trait Database extends Logging {
  protected def getConnection: Connection
  protected def statementBuilder: StatementBuilder

  private val transactionConnection = new DynamicVariable[Option[Connection]](None)

  def executeSelect[A](select: Select[_, _])(extractor: ResultSet => A): A =
    executeWithConnection { connection =>
      val preparedStatement = statementBuilder(connection, select)
      try {
        logger.debug(s"Executing select")
        val resultSet = preparedStatement.executeQuery

        try {
          logger.debug(s"Extracting results")
          extractor(resultSet)
        } finally {
          try {
            if (resultSet != null) resultSet.close
          } catch {
            case e: SQLException =>
          }
        }

      } finally {
        try {
          if (preparedStatement != null) preparedStatement.close
        } catch {
          case e: SQLException =>
        }
      }
    }

  def executeInsert(insert: Insert): Int = {
    logger.debug(s"Executing insert")
    executeCommand(insert)
  }

  def executeUpdate(update: Update): Int = {
    logger.debug(s"Executing update")
    executeCommand(update)
  }

  def executeDelete(delete: Delete): Int = {
    logger.debug(s"Executing delete")
    executeCommand(delete)
  }

  private def executeCommand(command: Command): Int = {
    checkInTransaction
    executeWithConnection { connection =>
      val preparedStatement = statementBuilder(connection, command)
      try {
        logger.debug(s"Executing command")
        preparedStatement.executeBatch.sum
      } finally {
        try {
          if (preparedStatement != null) preparedStatement.close
        } catch {
          case e: SQLException =>
        }
      }
    }
  }

  def executeBatch(batchCommands: Seq[Command]): List[Int] = {
    checkInTransaction
    executeWithConnection { connection =>
      val statement = connection.createStatement
      try {
        batchCommands foreach { command =>
          val commandSql = statementBuilder.generateRawSql(command)
          logger.debug(s"Adding batch operation: $commandSql")
          statement.addBatch(commandSql)
        }

        statement.executeBatch.toList
      } finally {
        try {
          if (statement != null) statement.close
        } catch {
          case e: SQLException =>
        }
      }
    }
  }

  def executeWithConnection[A](thunk: Connection => A): A =
    transactionConnection.value match {
      case Some(connection) => thunk(connection)
      case None =>
        val connection = getConnection
        try {
          thunk(connection)
        } finally {
          try {
            if (connection != null) connection.close
          } catch {
            case e: SQLException =>
          }
        }
    }

  def withTransaction[A](transaction: => A): A =
    if (transactionConnection.value.isDefined)
      // Already inside transaction so just run thunk
      transaction
    else
      executeWithNewTransaction(transaction)

  private def executeWithNewTransaction[A](transaction: => A): A = {
    val connection = getConnection
    try {
      try {
        connection.setAutoCommit(false)
        transactionConnection.withValue(Some(connection)) {
          val result = transaction
          connection.commit
          result
        }
      } catch {
        case e: SQLException =>
          connection.rollback
          throw e
      }
    } finally {
      try {
        if (connection != null) {
          connection.setAutoCommit(true)
          connection.close
        }
      } catch {
        case e: SQLException =>
      }
    }
  }

  private def checkInTransaction =
    if (transactionConnection.value.isEmpty)
      throw new AssertionError("Must run write operations in a transaction")
}

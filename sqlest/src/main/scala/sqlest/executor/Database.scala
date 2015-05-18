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
import sqlest.sql.base._
import sqlest.util.Logging

import java.sql.{ Connection, Date => JdbcDate, DriverManager, ResultSet, PreparedStatement, Statement, SQLException, Timestamp => JdbcTimestamp, Types => JdbcTypes }
import javax.sql.DataSource
import org.joda.time.{ DateTime, LocalDate }
import scala.util.DynamicVariable

object Database {
  def withDataSource(dataSource: DataSource, builder: StatementBuilder): Database = new Database {
    def getConnection: Connection = dataSource.getConnection
    val statementBuilder = builder
  }

  def withDataSource(dataSource: DataSource, builder: StatementBuilder, connectionDescription: Connection => String): Database = {
    val inConnectionDescription = connectionDescription
    new Database {
      def getConnection: Connection = dataSource.getConnection
      val statementBuilder = builder
      override val connectionDescription = Some(inConnectionDescription)
    }
  }
}

trait Database extends Logging {
  protected def getConnection: Connection
  protected def statementBuilder: StatementBuilder
  protected def connectionDescription: Option[Connection => String] = None

  private val transactionConnection = new DynamicVariable[Option[Connection]](None)

  def executeSelect[A](select: Select[_, _])(extractor: ResultSet => A): A =
    executeWithConnection { connection =>
      val (preprocessedSelect, sql, argumentLists) = statementBuilder(select)
      try {
        val startTime = new DateTime
        val preparedStatement = prepareStatement(connection, preprocessedSelect, sql, argumentLists)
        try {
          val resultSet = preparedStatement.executeQuery
          try {
            val result = extractor(resultSet)
            val endTime = new DateTime
            logger.info(s"Ran sql in ${endTime.getMillis - startTime.getMillis}ms: ${logDetails(connection, sql, argumentLists)}")
            result
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
      } catch {
        case e: Throwable =>
          logger.error(s"Exception running sql: ${logDetails(connection, sql, argumentLists)}", e)
          throw e
      }
    }

  def executeCommand(command: Command): Int = {
    checkInTransaction
    executeWithConnection { connection =>
      val (preprocessedCommand, sql, argumentLists) = statementBuilder(command)
      val startTime = new DateTime
      try {
        val preparedStatement = prepareStatement(connection, preprocessedCommand, sql, argumentLists)
        try {
          val result = preparedStatement.executeBatch.sum
          val endTime = new DateTime
          logger.info(s"Ran sql in ${endTime.getMillis - startTime.getMillis}ms: ${logDetails(connection, sql, argumentLists)}")
          result
        } finally {
          try {
            if (preparedStatement != null) preparedStatement.close
          } catch {
            case e: SQLException =>
          }
        }
      } catch {
        case e: Throwable =>
          logger.error(s"Exception running sql: ${logDetails(connection, sql, argumentLists)}", e)
          throw e
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

  def prepareStatement(connection: Connection, operation: Operation, sql: String, argumentLists: List[List[LiteralColumn[_]]]) = {
    val statement = connection.prepareStatement(sql)
    setArguments(operation, statement, argumentLists)
    statement
  }

  private def setArguments(operation: Operation, statement: PreparedStatement, argumentLists: List[List[LiteralColumn[_]]]) = {
    def innerSetArguments(argumentList: List[LiteralColumn[_]]) = {
      var index = 0
      argumentList foreach { argument =>
        index = index + 1 // prepared statement argument indices are 1-based
        setArgument(statement, index, argument.columnType, argument.value)
      }
    }

    argumentLists.foreach {
      argumentList =>
        innerSetArguments(argumentList)
        statement.addBatch
    }
  }

  private def setArgument[A](statement: PreparedStatement, index: Int, columnType: ColumnType[A], value: Any): Unit = columnType match {
    case BooleanColumnType => statement.setBoolean(index, value.asInstanceOf[Boolean])
    case IntColumnType => statement.setInt(index, value.asInstanceOf[Int])
    case LongColumnType => statement.setLong(index, value.asInstanceOf[Long])
    case DoubleColumnType => statement.setDouble(index, value.asInstanceOf[Double])
    case BigDecimalColumnType => statement.setBigDecimal(index, value.asInstanceOf[BigDecimal].bigDecimal)
    case StringColumnType => statement.setString(index, value.asInstanceOf[String])
    case ByteArrayColumnType => statement.setBytes(index, value.asInstanceOf[Array[Byte]])
    case DateTimeColumnType => statement.setTimestamp(index, new JdbcTimestamp(value.asInstanceOf[DateTime].getMillis))
    case LocalDateColumnType => statement.setDate(index, new JdbcDate(value.asInstanceOf[LocalDate].toDate.getTime))
    case mappedType: MappedColumnType[A, _] => setArgument(statement, index, mappedType.baseColumnType, mappedType.write(value.asInstanceOf[A]))
    case optionType: OptionColumnType[_, _] => value.asInstanceOf[Option[_]] match {
      case setNullOpt if setNullOpt.isEmpty && optionType.nullValue == null =>
        statement.setNull(index, jdbcType(optionType.baseColumnType))
      case nullValueOpt if nullValueOpt.isEmpty =>
        setArgument(statement, index, optionType.baseColumnType, optionType.nullValue)
      case definedOpt =>
        setArgument(statement, index, optionType.innerColumnType, definedOpt.get)
    }
  }

  private def jdbcType[A](columnType: ColumnType[A]): Int = columnType match {
    case BooleanColumnType => JdbcTypes.BOOLEAN
    case IntColumnType => JdbcTypes.INTEGER
    case LongColumnType => JdbcTypes.INTEGER
    case DoubleColumnType => JdbcTypes.DOUBLE
    case BigDecimalColumnType => JdbcTypes.DECIMAL
    case StringColumnType => JdbcTypes.CHAR
    case ByteArrayColumnType => JdbcTypes.BINARY
    case DateTimeColumnType => JdbcTypes.TIMESTAMP
    case LocalDateColumnType => JdbcTypes.DATE
    case optionType: OptionColumnType[_, _] => jdbcType(optionType.baseColumnType)
    case mappedType: MappedColumnType[_, _] => jdbcType(mappedType.baseColumnType)
  }

  def logDetails(connection: Connection, sql: String, argumentLists: List[List[LiteralColumn[_]]]) = {
    val connectionLog = connectionDescription.map(connectionDescription => s", connection [${connectionDescription(connection)}]").getOrElse("")
    val argumentsLog =
      if (argumentLists.size == 1) argumentLists.head.map(_.value).mkString(", ")
      else argumentLists.map(_.map(_.value).mkString("(", ", ", ")")).mkString(", ")

    s"sql [$sql], arguments [$argumentsLog]${connectionLog}"
  }
}

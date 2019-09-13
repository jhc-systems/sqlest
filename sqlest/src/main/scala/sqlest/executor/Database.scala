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
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ DynamicVariable, Try, Success, Failure }
import scala.util.control.NonFatal
import sqlest.extractor.IndexedExtractor

object Database {
  def withDataSource(
    dataSource: DataSource,
    builder: StatementBuilder
  ): Database = new Database {
    def getConnection: Connection = dataSource.getConnection
    val statementBuilder = builder
  }

  def withDataSource(
    dataSource: DataSource,
    builder: StatementBuilder,
    verboseExceptionMessages: Boolean
  ): Database = new Database {
    def getConnection: Connection = dataSource.getConnection
    val statementBuilder = builder
    override val verboseExceptions = verboseExceptionMessages
  }

  def withDataSource(
    dataSource: DataSource,
    builder: StatementBuilder,
    connectionDescription: Connection => String
  ): Database = {
    val inConnectionDescription = connectionDescription

    new Database {
      def getConnection: Connection = dataSource.getConnection
      val statementBuilder = builder

      override val connectionDescription = Some(inConnectionDescription)
    }
  }

  def withDataSource(
    dataSource: DataSource,
    builder: StatementBuilder,
    connectionDescription: Connection => String,
    verboseExceptionMessages: Boolean
  ): Database = {
    val inConnectionDescription = connectionDescription
    new Database {
      def getConnection: Connection = dataSource.getConnection
      val statementBuilder = builder
      override val verboseExceptions = verboseExceptionMessages
      override val connectionDescription = Some(inConnectionDescription)
    }
  }

  def withDataSource(
    dataSource: DataSource,
    builder: StatementBuilder,
    connectionDescription: Connection => String,
    verboseExceptionMessages: Boolean,
    queryTimeoutValue: Integer
  ): Database = {
    val inConnectionDescription = connectionDescription
    new Database {
      def getConnection: Connection = dataSource.getConnection
      val statementBuilder = builder
      override val verboseExceptions = verboseExceptionMessages
      override val queryTimeout = queryTimeoutValue
      override val connectionDescription = Some(inConnectionDescription)
    }
  }

}

trait Database {
  private[sqlest] def getConnection: Connection
  private[sqlest] def statementBuilder: StatementBuilder
  private[sqlest] def connectionDescription: Option[Connection => String] = None
  private[sqlest] def verboseExceptions: Boolean = false
  private[sqlest] def queryTimeout: Integer = 0

  def withConnection[A](f: Connection => A): A =
    Session(this).withConnection(f)

  def withSession[A](f: Session => A): A =
    f(Session(this))

  def withTransaction[A](f: Transaction => A): A =
    Transaction(this).run(f)

  def withTransactionAsync[A](f: Transaction => Future[A])(implicit ec: ExecutionContext): Future[A] =
    Transaction(this).runAsync(f)
}

object Session {
  def apply(database: Database) = new Session(database)
  implicit def databaseToSession(implicit database: Database) = Session(database)
}

class Session(database: Database) extends Logging {

  def withConnection[A](f: Connection => A): A = {
    val connection = database.getConnection
    try {
      f(connection)
    } finally {
      try {
        connection.close
      } catch { case e: SQLException => }
    }
  }

  def executeSelect[A](select: Select[_, _])(extractor: ResultSet => A): A =
    withConnection { connection =>
      val (preprocessedSelect, sql, argumentLists) = database.statementBuilder(select)
      val startTime = new DateTime
      try {
        val preparedStatement = prepareStatement(connection, preprocessedSelect, sql, argumentLists)
        try {
          preparedStatement.setQueryTimeout(database.queryTimeout)
          val resultSet = preparedStatement.executeQuery
          try {
            val result = extractor(resultSet)
            val endTime = new DateTime
            logger.info(s"Ran sql in ${endTime.getMillis - startTime.getMillis}ms: ${logDetails(connection, sql, argumentLists)} with queryTimeout ${database.queryTimeout}")
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
        case NonFatal(e) =>
          if (!database.verboseExceptions)
            throw e
          else
            throw new SqlestException(s"Exception running sql: ${logDetails(connection, sql, argumentLists)}", e)
      }
    }

  protected def prepareStatement(connection: Connection, operation: Operation, sql: String, argumentLists: List[List[LiteralColumn[_]]]): PreparedStatement = {
    prepareStatement(connection, operation, sql, argumentLists, returnKeys = false)
  }

  protected def prepareStatement(connection: Connection, operation: Operation, sql: String, argumentLists: List[List[LiteralColumn[_]]], returnKeys: Boolean): PreparedStatement = {
    val statement =
      if (returnKeys)
        connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
      else connection.prepareStatement(sql)
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
      case None if optionType.hasNullNullValue =>
        statement.setNull(index, jdbcType(optionType.baseColumnType))
      case None =>
        setArgument(statement, index, optionType.baseColumnType, optionType.nullValue)
      case Some(inner) =>
        setArgument(statement, index, optionType.innerColumnType, inner)
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

  protected def logDetails(connection: Connection, sql: String, argumentLists: List[List[LiteralColumn[_]]]) = {
    val connectionLog = database.connectionDescription.map(connectionDescription => s", connection [${connectionDescription(connection)}]").getOrElse("")
    val argumentsLog =
      if (argumentLists.size == 1) argumentLists.head.map(_.value).mkString(", ")
      else argumentLists.map(_.map(_.value).mkString("(", ", ", ")")).mkString(", ")

    s"sql [$sql], arguments [$argumentsLog]${connectionLog}"
  }
}

case class Transaction(database: Database) extends Session(database) {
  private var shouldRollback = false
  def rollback = shouldRollback = true
  private lazy val connection = database.getConnection

  // Run all sql in the same connection in a transaction
  override def withConnection[A](f: Connection => A): A =
    f(connection)

  private[sqlest] def run[A](f: Transaction => A): A =
    try {
      connection.setAutoCommit(false)
      try {
        var success = false
        try {
          val result = f(this)
          if (shouldRollback) connection.rollback
          else connection.commit
          success = true
          result
        } finally if (!success) connection.rollback
      } finally {
        connection.setAutoCommit(true)
      }
    } finally {
      try {
        connection.close
      } catch { case e: SQLException => }
    }

  private[sqlest] def runAsync[A](f: Transaction => Future[A])(implicit ec: ExecutionContext): Future[A] = {
    val runResult = try {
      connection.setAutoCommit(false)
      val connectionResult = try {
        var success = false

        val functionResult = try {
          val functionCall = f(this)
          functionCall andThen {
            case Success(_) =>
              if (shouldRollback) connection.rollback
              else connection.commit
              success = true
              functionCall
            case Failure(_) =>
              connection.rollback
              functionCall
          }
        } catch {
          case NonFatal(e) => Future.failed(e)
        }

        functionResult andThen {
          case _ =>
            if (!success) connection.rollback
            functionResult
        }
      } catch {
        case NonFatal(e) => Future.failed(e)
      }

      connectionResult andThen {
        case _ =>
          connection.setAutoCommit(true)
          connectionResult
      }
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
    runResult andThen {
      case _ =>
        try {
          connection.close
        } catch { case e: SQLException => }
        runResult
    }
  }

  def executeCommand(command: Command): Int =
    withConnection { connection =>
      val (preprocessedCommand, sql, argumentLists) = database.statementBuilder(command)
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
          } catch { case e: SQLException => }
        }
      } catch {
        case NonFatal(e) =>
          if (!database.verboseExceptions)
            throw e
          else
            throw new SqlestException(s"Exception running sql: ${logDetails(connection, sql, argumentLists)}", e)
      }
    }

  def executeInsertReturningKeys[T](command: Insert)(implicit columnType: ColumnType[T]): List[T] =
    withConnection { connection =>
      val (preprocessedCommand, sql, argumentLists) = database.statementBuilder(command)
      val startTime = new DateTime
      try {
        val preparedStatement = prepareStatement(
          connection,
          preprocessedCommand,
          sql,
          argumentLists,
          returnKeys = true
        )
        try {
          val result = preparedStatement.executeUpdate
          val rs = preparedStatement.getGeneratedKeys
          val keys = IndexedExtractor[T](1).extractAll(ResultSetIterable(rs))
          val endTime = new DateTime
          logger.info(s"Ran sql in ${endTime.getMillis - startTime.getMillis}ms: ${logDetails(connection, sql, argumentLists)}")
          keys
        } finally {
          try {
            if (preparedStatement != null) preparedStatement.close
          } catch {
            case e: SQLException =>
          }
        }
      } catch {
        case NonFatal(e) =>
          if (!database.verboseExceptions)
            throw e
          else
            throw new SqlestException(s"Exception running sql: ${logDetails(connection, sql, argumentLists)}", e)
      }
    }

  def executeBatch(batchCommands: Seq[Command]): List[Int] =
    withConnection { connection =>
      val statement = connection.createStatement
      try {
        batchCommands foreach { command =>
          val commandSql = database.statementBuilder.generateRawSql(command)
          logger.debug(s"Adding batch operation: $commandSql")
          statement.addBatch(commandSql)
        }

        statement.executeBatch.toList
      } finally {
        try {
          if (statement != null) statement.close
        } catch { case e: SQLException => }
      }
    }
}

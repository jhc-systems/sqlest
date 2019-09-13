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

import java.sql.ResultSet
import sqlest._

case class TestDatabase(resultSet: ResultSet, keyResultSet: Option[ResultSet] = None, shouldThrow: Boolean = false, verboseExceptionMessages: Boolean = false, queryTimeoutValue: Int = 0, throwExceptionOnPrepare: Boolean = false) extends Database {
  var preparedStatement: Option[AbstractPreparedStatement] = None
  var lastConnection: Option[AbstractConnection] = None
  override val verboseExceptions = verboseExceptionMessages
  override val queryTimeout = queryTimeoutValue

  val anException = new Exception("Oh noes!")

  def statementBuilder: StatementBuilder = sqlest.sql.H2StatementBuilder

  def getConnection = {
    val connection = new AbstractConnection {
      var closed = false
      var committed = false
      var rolledBack = false
      override def close = { closed = true }
      override def commit = { committed = true }
      override def rollback = {
        if (closed)
          throw new Exception("Tried to roll back a closed connection")
        else rolledBack = true
      }
      override def setAutoCommit(autoCommit: Boolean) = {}
      override def createStatement() = new AbstractPreparedStatement {
        val sql = null
        override def execute(sql: String) = if (shouldThrow) throw anException else true
      }
      override def prepareStatement(inSql: String, columnIndices: Array[Int]) = {

        val statement = if (throwExceptionOnPrepare) throw anException else new AbstractPreparedStatement {
          val sql = inSql
          override def executeQuery() = if (shouldThrow) throw anException else resultSet
          override def executeUpdate() = if (shouldThrow) throw anException else 1
          override def executeBatch() = if (shouldThrow) throw anException else Array()
          override def getGeneratedKeys(): java.sql.ResultSet = {
            keyResultSet.getOrElse(null)
          }
        }
        preparedStatement = Some(statement)
        statement
      }
      override def prepareStatement(inSql: String, columnNames: Array[String]) = {
        val statement = if (throwExceptionOnPrepare) throw anException else new AbstractPreparedStatement {
          val sql = inSql
          override def executeQuery() = if (shouldThrow) throw anException else resultSet
          override def executeUpdate() = if (shouldThrow) throw anException else 1
          override def executeBatch() = if (shouldThrow) throw anException else Array()
          override def getGeneratedKeys(): java.sql.ResultSet = {
            keyResultSet.getOrElse(null)
          }
        }
        preparedStatement = Some(statement)
        statement
      }
      override def prepareStatement(inSql: String, returnGeneratedKeys: Int) = {
        val statement = if (throwExceptionOnPrepare) throw anException else new AbstractPreparedStatement {
          val sql = inSql
          override def executeQuery() = if (shouldThrow) throw anException else resultSet
          override def executeUpdate() = if (shouldThrow) throw anException else 1
          override def executeBatch() = if (shouldThrow) throw anException else Array()
          override def getGeneratedKeys(): java.sql.ResultSet = {
            if (returnGeneratedKeys == java.sql.Statement.RETURN_GENERATED_KEYS)
              keyResultSet.getOrElse(null)
            else null
          }
        }
        preparedStatement = Some(statement)
        statement
      }
      override def prepareStatement(inSql: String) = {
        val statement = if (throwExceptionOnPrepare) throw anException else new AbstractPreparedStatement {
          val sql = inSql
          override def executeQuery() = if (shouldThrow) throw anException else resultSet
          override def executeUpdate() = if (shouldThrow) throw anException else 1
          override def executeBatch() = if (shouldThrow) throw anException else Array()
          override def getGeneratedKeys(): java.sql.ResultSet = null
        }
        preparedStatement = Some(statement)
        statement
      }
    }
    lastConnection = Some(connection)
    connection
  }
}


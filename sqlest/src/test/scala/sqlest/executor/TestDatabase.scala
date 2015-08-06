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

case class TestDatabase(resultSet: ResultSet) extends Database {
  var preparedStatement: Option[AbstractPreparedStatement] = None

  def statementBuilder: StatementBuilder = sqlest.sql.H2StatementBuilder

  def getConnection = new AbstractConnection {
    override def close = {}
    override def commit = {}
    override def rollback = {}
    override def setAutoCommit(autoCommit: Boolean) = {}
    override def prepareStatement(inSql: String) = {
      val statement = new AbstractPreparedStatement {
        val sql = inSql
        override def executeQuery() = resultSet
        override def executeUpdate() = 0
        override def executeBatch() = Array()
      }
      preparedStatement = Some(statement)
      statement
    }
  }
}


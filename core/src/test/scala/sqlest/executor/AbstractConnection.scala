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

import java.sql.Connection

trait AbstractConnection extends Connection {
  def abort(x$1: java.util.concurrent.Executor): Unit = ???
  def clearWarnings(): Unit = ???
  def close(): Unit = ???
  def commit(): Unit = ???
  def createArrayOf(x$1: String, x$2: Array[Object]): java.sql.Array = ???
  def createBlob(): java.sql.Blob = ???
  def createClob(): java.sql.Clob = ???
  def createNClob(): java.sql.NClob = ???
  def createSQLXML(): java.sql.SQLXML = ???
  def createStatement(x$1: Int, x$2: Int, x$3: Int): java.sql.Statement = ???
  def createStatement(x$1: Int, x$2: Int): java.sql.Statement = ???
  def createStatement(): java.sql.Statement = ???
  def createStruct(x$1: String, x$2: Array[Object]): java.sql.Struct = ???
  def getAutoCommit(): Boolean = ???
  def getCatalog(): String = ???
  def getClientInfo(): java.util.Properties = ???
  def getClientInfo(x$1: String): String = ???
  def getHoldability(): Int = ???
  def getMetaData(): java.sql.DatabaseMetaData = ???
  def getNetworkTimeout(): Int = ???
  def getSchema(): String = ???
  def getTransactionIsolation(): Int = ???
  def getTypeMap(): java.util.Map[String, Class[_]] = ???
  def getWarnings(): java.sql.SQLWarning = ???
  def isClosed(): Boolean = ???
  def isReadOnly(): Boolean = ???
  def isValid(x$1: Int): Boolean = ???
  def isWrapperFor(x$1: Class[_]): Boolean = ???
  def nativeSQL(x$1: String): String = ???
  def prepareCall(x$1: String, x$2: Int, x$3: Int, x$4: Int): java.sql.CallableStatement = ???
  def prepareCall(x$1: String, x$2: Int, x$3: Int): java.sql.CallableStatement = ???
  def prepareCall(x$1: String): java.sql.CallableStatement = ???
  def prepareStatement(x$1: String, x$2: Array[String]): java.sql.PreparedStatement = ???
  def prepareStatement(x$1: String, x$2: Array[Int]): java.sql.PreparedStatement = ???
  def prepareStatement(x$1: String, x$2: Int): java.sql.PreparedStatement = ???
  def prepareStatement(x$1: String, x$2: Int, x$3: Int, x$4: Int): java.sql.PreparedStatement = ???
  def prepareStatement(x$1: String, x$2: Int, x$3: Int): java.sql.PreparedStatement = ???
  def prepareStatement(x$1: String): java.sql.PreparedStatement = ???
  def releaseSavepoint(x$1: java.sql.Savepoint): Unit = ???
  def rollback(x$1: java.sql.Savepoint): Unit = ???
  def rollback(): Unit = ???
  def setAutoCommit(x$1: Boolean): Unit = ???
  def setCatalog(x$1: String): Unit = ???
  def setClientInfo(x$1: java.util.Properties): Unit = ???
  def setClientInfo(x$1: String, x$2: String): Unit = ???
  def setHoldability(x$1: Int): Unit = ???
  def setNetworkTimeout(x$1: java.util.concurrent.Executor, x$2: Int): Unit = ???
  def setReadOnly(x$1: Boolean): Unit = ???
  def setSavepoint(x$1: String): java.sql.Savepoint = ???
  def setSavepoint(): java.sql.Savepoint = ???
  def setSchema(x$1: String): Unit = ???
  def setTransactionIsolation(x$1: Int): Unit = ???
  def setTypeMap(x$1: java.util.Map[String, Class[_]]): Unit = ???
  def unwrap[T](x$1: Class[T]): T = ???
}
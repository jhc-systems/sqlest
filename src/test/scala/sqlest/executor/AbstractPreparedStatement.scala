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

import java.sql.PreparedStatement

trait AbstractPreparedStatement extends PreparedStatement {
  // Prepared statment methods
  def addBatch(): Unit = {}
  def clearParameters(): Unit = {}
  def execute(): Boolean = ???
  def executeQuery(): java.sql.ResultSet = ???
  def executeUpdate(): Int = ???
  def getMetaData(): java.sql.ResultSetMetaData = ???
  def getParameterMetaData(): java.sql.ParameterMetaData = ???
  def isWrapperFor(x$1: Class[_]): Boolean = ???
  def setArray(x$1: Int, x$2: java.sql.Array): Unit = {}
  def setAsciiStream(x$1: Int, x$2: java.io.InputStream): Unit = {}
  def setAsciiStream(x$1: Int, x$2: java.io.InputStream, x$3: Long): Unit = {}
  def setAsciiStream(x$1: Int, x$2: java.io.InputStream, x$3: Int): Unit = {}
  def setBigDecimal(x$1: Int, x$2: java.math.BigDecimal): Unit = {}
  def setBinaryStream(x$1: Int, x$2: java.io.InputStream): Unit = {}
  def setBinaryStream(x$1: Int, x$2: java.io.InputStream, x$3: Long): Unit = {}
  def setBinaryStream(x$1: Int, x$2: java.io.InputStream, x$3: Int): Unit = {}
  def setBlob(x$1: Int, x$2: java.io.InputStream): Unit = {}
  def setBlob(x$1: Int, x$2: java.io.InputStream, x$3: Long): Unit = {}
  def setBlob(x$1: Int, x$2: java.sql.Blob): Unit = {}
  def setBoolean(x$1: Int, x$2: Boolean): Unit = {}
  def setByte(x$1: Int, x$2: Byte): Unit = {}
  def setBytes(x$1: Int, x$2: Array[Byte]): Unit = {}
  def setCharacterStream(x$1: Int, x$2: java.io.Reader): Unit = {}
  def setCharacterStream(x$1: Int, x$2: java.io.Reader, x$3: Long): Unit = {}
  def setCharacterStream(x$1: Int, x$2: java.io.Reader, x$3: Int): Unit = {}
  def setClob(x$1: Int, x$2: java.io.Reader): Unit = {}
  def setClob(x$1: Int, x$2: java.io.Reader, x$3: Long): Unit = {}
  def setClob(x$1: Int, x$2: java.sql.Clob): Unit = {}
  def setDate(x$1: Int, x$2: java.sql.Date, x$3: java.util.Calendar): Unit = {}
  def setDate(x$1: Int, x$2: java.sql.Date): Unit = {}
  def setDouble(x$1: Int, x$2: Double): Unit = {}
  def setFloat(x$1: Int, x$2: Float): Unit = {}
  def setInt(x$1: Int, x$2: Int): Unit = {}
  def setLong(x$1: Int, x$2: Long): Unit = {}
  def setNCharacterStream(x$1: Int, x$2: java.io.Reader): Unit = {}
  def setNCharacterStream(x$1: Int, x$2: java.io.Reader, x$3: Long): Unit = {}
  def setNClob(x$1: Int, x$2: java.io.Reader): Unit = {}
  def setNClob(x$1: Int, x$2: java.io.Reader, x$3: Long): Unit = {}
  def setNClob(x$1: Int, x$2: java.sql.NClob): Unit = {}
  def setNString(x$1: Int, x$2: String): Unit = {}
  def setNull(x$1: Int, x$2: Int, x$3: String): Unit = {}
  def setNull(x$1: Int, x$2: Int): Unit = {}
  def setObject(x$1: Int, x$2: Any, x$3: Int, x$4: Int): Unit = {}
  def setObject(x$1: Int, x$2: Any): Unit = {}
  def setObject(x$1: Int, x$2: Any, x$3: Int): Unit = {}
  def setRef(x$1: Int, x$2: java.sql.Ref): Unit = {}
  def setRowId(x$1: Int, x$2: java.sql.RowId): Unit = {}
  def setSQLXML(x$1: Int, x$2: java.sql.SQLXML): Unit = {}
  def setShort(x$1: Int, x$2: Short): Unit = {}
  def setString(x$1: Int, x$2: String): Unit = {}
  def setTime(x$1: Int, x$2: java.sql.Time, x$3: java.util.Calendar): Unit = {}
  def setTime(x$1: Int, x$2: java.sql.Time): Unit = {}
  def setTimestamp(x$1: Int, x$2: java.sql.Timestamp, x$3: java.util.Calendar): Unit = {}
  def setTimestamp(x$1: Int, x$2: java.sql.Timestamp): Unit = {}
  def setURL(x$1: Int, x$2: java.net.URL): Unit = {}
  def setUnicodeStream(x$1: Int, x$2: java.io.InputStream, x$3: Int): Unit = {}
  def unwrap[T](x$1: Class[T]): T = ???

  // Statement methods
  def addBatch(x$1: String): Unit = {}
  def cancel(): Unit = {}
  def clearBatch(): Unit = {}
  def clearWarnings(): Unit = {}
  def close(): Unit = {}
  def closeOnCompletion(): Unit = {}
  def execute(x$1: String, x$2: Array[String]): Boolean = ???
  def execute(x$1: String, x$2: Array[Int]): Boolean = ???
  def execute(x$1: String, x$2: Int): Boolean = ???
  def execute(x$1: String): Boolean = ???
  def executeBatch(): Array[Int] = ???
  def executeQuery(x$1: String): java.sql.ResultSet = ???
  def executeUpdate(x$1: String, x$2: Array[String]): Int = ???
  def executeUpdate(x$1: String, x$2: Array[Int]): Int = ???
  def executeUpdate(x$1: String, x$2: Int): Int = ???
  def executeUpdate(x$1: String): Int = ???
  def getConnection(): java.sql.Connection = ???
  def getFetchDirection(): Int = ???
  def getFetchSize(): Int = ???
  def getGeneratedKeys(): java.sql.ResultSet = ???
  def getMaxFieldSize(): Int = ???
  def getMaxRows(): Int = ???
  def getMoreResults(x$1: Int): Boolean = ???
  def getMoreResults(): Boolean = ???
  def getQueryTimeout(): Int = ???
  def getResultSet(): java.sql.ResultSet = ???
  def getResultSetConcurrency(): Int = ???
  def getResultSetHoldability(): Int = ???
  def getResultSetType(): Int = ???
  def getUpdateCount(): Int = ???
  def getWarnings(): java.sql.SQLWarning = ???
  def isCloseOnCompletion(): Boolean = ???
  def isClosed(): Boolean = ???
  def isPoolable(): Boolean = ???
  def setCursorName(x$1: String): Unit = {}
  def setEscapeProcessing(x$1: Boolean): Unit = {}
  def setFetchDirection(x$1: Int): Unit = {}
  def setFetchSize(x$1: Int): Unit = {}
  def setMaxFieldSize(x$1: Int): Unit = {}
  def setMaxRows(x$1: Int): Unit = {}
  def setPoolable(x$1: Boolean): Unit = {}
  def setQueryTimeout(x$1: Int): Unit = {}
}

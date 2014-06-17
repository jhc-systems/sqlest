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

package sqlest.extractor

import java.io.InputStream
import java.io.Reader
import java.math.BigDecimal
import java.net.URL
import java.sql.{ Array => SqlArray }
import java.sql.Blob
import java.sql.Clob
import java.sql.Date
import java.sql.NClob
import java.sql.Ref
import java.sql.ResultSet
import java.sql.ResultSetMetaData
import java.sql.RowId
import java.sql.SQLException
import java.sql.SQLWarning
import java.sql.SQLXML
import java.sql.Statement
import java.sql.Time
import java.sql.Timestamp
import java.util.Calendar
import java.util.Map

trait AbstractResultSet extends ResultSet {
  def isWrapperFor(klass: Class[_]): Boolean = ???
  def unwrap[T](klass: Class[T]): T = ???
  def absolute(row: Int): Boolean = ???
  def afterLast(): Unit = {}
  def beforeFirst(): Unit = {}
  def cancelRowUpdates(): Unit = {}
  def clearWarnings(): Unit = {}
  def close(): Unit = {}
  def deleteRow(): Unit = {}
  def findColumn(columnLabel: String): Int = ???
  def first(): Boolean = ???
  def getArray(columnIndex: Int): SqlArray = ???
  def getArray(columnLabel: String): SqlArray = ???
  def getAsciiStream(columnIndex: Int): InputStream = ???
  def getAsciiStream(columnLabel: String): InputStream = ???
  def getBigDecimal(columnIndex: Int, scale: Int): BigDecimal = ???
  def getBigDecimal(columnIndex: Int): BigDecimal = ???
  def getBigDecimal(columnLabel: String, scale: Int): BigDecimal = ???
  def getBigDecimal(columnLabel: String): BigDecimal = ???
  def getBinaryStream(columnIndex: Int): InputStream = ???
  def getBinaryStream(columnLabel: String): InputStream = ???
  def getBlob(columnIndex: Int): Blob = ???
  def getBlob(columnLabel: String): Blob = ???
  def getBoolean(columnIndex: Int): Boolean = ???
  def getBoolean(columnLabel: String): Boolean = ???
  def getByte(columnIndex: Int): Byte = ???
  def getByte(columnLabel: String): Byte = ???
  def getBytes(columnIndex: Int): Array[Byte] = ???
  def getBytes(columnLabel: String): Array[Byte] = ???
  def getCharacterStream(columnIndex: Int): Reader = ???
  def getCharacterStream(columnLabel: String): Reader = ???
  def getClob(columnIndex: Int): Clob = ???
  def getClob(columnLabel: String): Clob = ???
  def getConcurrency(): Int = ???
  def getCursorName(): String = ???
  def getDate(columnIndex: Int, cal: Calendar): Date = ???
  def getDate(columnIndex: Int): Date = ???
  def getDate(columnLabel: String, cal: Calendar): Date = ???
  def getDate(columnLabel: String): Date = ???
  def getDouble(columnIndex: Int): Double = ???
  def getDouble(columnLabel: String): Double = ???
  def getFetchDirection(): Int = ???
  def getFetchSize(): Int = ???
  def getFloat(columnIndex: Int): Float = ???
  def getFloat(columnLabel: String): Float = ???
  def getHoldability(): Int = ???
  def getInt(columnIndex: Int): Int = ???
  def getInt(columnLabel: String): Int = ???
  def getLong(columnIndex: Int): Long = ???
  def getLong(columnLabel: String): Long = ???
  def getMetaData(): ResultSetMetaData = ???
  def getNCharacterStream(columnIndex: Int): Reader = ???
  def getNCharacterStream(columnLabel: String): Reader = ???
  def getNClob(columnIndex: Int): NClob = ???
  def getNClob(columnLabel: String): NClob = ???
  def getNString(columnIndex: Int): String = ???
  def getNString(columnLabel: String): String = ???
  def getObject[T](columnName: String, klass: Class[T]): T = ???
  def getObject[T](columnIndex: Int, klass: Class[T]): T = ???
  def getObject(columnIndex: Int, map: Map[String, Class[_]]): Object = ???
  def getObject(columnIndex: Int): Object = ???
  def getObject(columnLabel: String, map: Map[String, Class[_]]): Object = ???
  def getObject(columnLabel: String): Object = ???
  def getRef(columnIndex: Int): Ref = ???
  def getRef(columnLabel: String): Ref = ???
  def getRow(): Int = ???
  def getRowId(columnIndex: Int): RowId = ???
  def getRowId(columnLabel: String): RowId = ???
  def getSQLXML(columnIndex: Int): SQLXML = ???
  def getSQLXML(columnLabel: String): SQLXML = ???
  def getShort(columnIndex: Int): Short = ???
  def getShort(columnLabel: String): Short = ???
  def getStatement(): Statement = ???
  def getString(columnIndex: Int): String = ???
  def getString(columnLabel: String): String = ???
  def getTime(columnIndex: Int, cal: Calendar): Time = ???
  def getTime(columnIndex: Int): Time = ???
  def getTime(columnLabel: String, cal: Calendar): Time = ???
  def getTime(columnLabel: String): Time = ???
  def getTimestamp(columnIndex: Int, cal: Calendar): Timestamp = ???
  def getTimestamp(columnIndex: Int): Timestamp = ???
  def getTimestamp(columnLabel: String, cal: Calendar): Timestamp = ???
  def getTimestamp(columnLabel: String): Timestamp = ???
  def getType(): Int = ???
  def getURL(columnIndex: Int): URL = ???
  def getURL(columnLabel: String): URL = ???
  def getUnicodeStream(columnIndex: Int): InputStream = ???
  def getUnicodeStream(columnLabel: String): InputStream = ???
  def getWarnings(): SQLWarning = ???
  def insertRow(): Unit = {}
  def isAfterLast(): Boolean = ???
  def isBeforeFirst(): Boolean = ???
  def isClosed(): Boolean = ???
  def isFirst(): Boolean = ???
  def isLast(): Boolean = ???
  def last(): Boolean = ???
  def moveToCurrentRow(): Unit = {}
  def moveToInsertRow(): Unit = {}
  def next(): Boolean = ???
  def previous(): Boolean = ???
  def refreshRow(): Unit = {}
  def relative(rows: Int): Boolean = ???
  def rowDeleted(): Boolean = ???
  def rowInserted(): Boolean = ???
  def rowUpdated(): Boolean = ???
  def setFetchDirection(direction: Int): Unit = {}
  def setFetchSize(rows: Int): Unit = {}
  def updateArray(columnIndex: Int, x: SqlArray): Unit = {}
  def updateArray(columnLabel: String, x: SqlArray): Unit = {}
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Int): Unit = {}
  def updateAsciiStream(columnIndex: Int, x: InputStream, length: Long): Unit = {}
  def updateAsciiStream(columnIndex: Int, x: InputStream): Unit = {}
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Int): Unit = {}
  def updateAsciiStream(columnLabel: String, x: InputStream, length: Long): Unit = {}
  def updateAsciiStream(columnLabel: String, x: InputStream): Unit = {}
  def updateBigDecimal(columnIndex: Int, x: BigDecimal): Unit = {}
  def updateBigDecimal(columnLabel: String, x: BigDecimal): Unit = {}
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Int): Unit = {}
  def updateBinaryStream(columnIndex: Int, x: InputStream, length: Long): Unit = {}
  def updateBinaryStream(columnIndex: Int, x: InputStream): Unit = {}
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Int): Unit = {}
  def updateBinaryStream(columnLabel: String, x: InputStream, length: Long): Unit = {}
  def updateBinaryStream(columnLabel: String, x: InputStream): Unit = {}
  def updateBlob(columnIndex: Int, x: Blob): Unit = {}
  def updateBlob(columnIndex: Int, inputStream: InputStream, length: Long): Unit = {}
  def updateBlob(columnIndex: Int, inputStream: InputStream): Unit = {}
  def updateBlob(columnLabel: String, x: Blob): Unit = {}
  def updateBlob(columnLabel: String, inputStream: InputStream, length: Long): Unit = {}
  def updateBlob(columnLabel: String, inputStream: InputStream): Unit = {}
  def updateBoolean(columnIndex: Int, x: Boolean): Unit = {}
  def updateBoolean(columnLabel: String, x: Boolean): Unit = {}
  def updateByte(columnIndex: Int, x: Byte): Unit = {}
  def updateByte(columnLabel: String, x: Byte): Unit = {}
  def updateBytes(columnIndex: Int, x: Array[Byte]): Unit = {}
  def updateBytes(columnLabel: String, x: Array[Byte]): Unit = {}
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Int): Unit = {}
  def updateCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = {}
  def updateCharacterStream(columnIndex: Int, x: Reader): Unit = {}
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Int): Unit = {}
  def updateCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = {}
  def updateCharacterStream(columnLabel: String, reader: Reader): Unit = {}
  def updateClob(columnIndex: Int, x: Clob): Unit = {}
  def updateClob(columnIndex: Int, reader: Reader, length: Long): Unit = {}
  def updateClob(columnIndex: Int, reader: Reader): Unit = {}
  def updateClob(columnLabel: String, x: Clob): Unit = {}
  def updateClob(columnLabel: String, reader: Reader, length: Long): Unit = {}
  def updateClob(columnLabel: String, reader: Reader): Unit = {}
  def updateDate(columnIndex: Int, x: Date): Unit = {}
  def updateDate(columnLabel: String, x: Date): Unit = {}
  def updateDouble(columnIndex: Int, x: Double): Unit = {}
  def updateDouble(columnLabel: String, x: Double): Unit = {}
  def updateFloat(columnIndex: Int, x: Float): Unit = {}
  def updateFloat(columnLabel: String, x: Float): Unit = {}
  def updateInt(columnIndex: Int, x: Int): Unit = {}
  def updateInt(columnLabel: String, x: Int): Unit = {}
  def updateLong(columnIndex: Int, x: Long): Unit = {}
  def updateLong(columnLabel: String, x: Long): Unit = {}
  def updateNCharacterStream(columnIndex: Int, x: Reader, length: Long): Unit = {}
  def updateNCharacterStream(columnIndex: Int, x: Reader): Unit = {}
  def updateNCharacterStream(columnLabel: String, reader: Reader, length: Long): Unit = {}
  def updateNCharacterStream(columnLabel: String, reader: Reader): Unit = {}
  def updateNClob(columnIndex: Int, nClob: NClob): Unit = {}
  def updateNClob(columnIndex: Int, reader: Reader, length: Long): Unit = {}
  def updateNClob(columnIndex: Int, reader: Reader): Unit = {}
  def updateNClob(columnLabel: String, nClob: NClob): Unit = {}
  def updateNClob(columnLabel: String, reader: Reader, length: Long): Unit = {}
  def updateNClob(columnLabel: String, reader: Reader): Unit = {}
  def updateNString(columnIndex: Int, nString: String): Unit = {}
  def updateNString(columnLabel: String, nString: String): Unit = {}
  def updateNull(columnIndex: Int): Unit = {}
  def updateNull(columnLabel: String): Unit = {}
  def updateObject(columnIndex: Int, x: Object, scaleOrLength: Int): Unit = {}
  def updateObject(columnIndex: Int, x: Object): Unit = {}
  def updateObject(columnLabel: String, x: Object, scaleOrLength: Int): Unit = {}
  def updateObject(columnLabel: String, x: Object): Unit = {}
  def updateRef(columnIndex: Int, x: Ref): Unit = {}
  def updateRef(columnLabel: String, x: Ref): Unit = {}
  def updateRow(): Unit = {}
  def updateRowId(columnIndex: Int, x: RowId): Unit = {}
  def updateRowId(columnLabel: String, x: RowId): Unit = {}
  def updateSQLXML(columnIndex: Int, xmlObject: SQLXML): Unit = {}
  def updateSQLXML(columnLabel: String, xmlObject: SQLXML): Unit = {}
  def updateShort(columnIndex: Int, x: Short): Unit = {}
  def updateShort(columnLabel: String, x: Short): Unit = {}
  def updateString(columnIndex: Int, x: String): Unit = {}
  def updateString(columnLabel: String, x: String): Unit = {}
  def updateTime(columnIndex: Int, x: Time): Unit = {}
  def updateTime(columnLabel: String, x: Time): Unit = {}
  def updateTimestamp(columnIndex: Int, x: Timestamp): Unit = {}
  def updateTimestamp(columnLabel: String, x: Timestamp): Unit = {}
  def wasNull(): Boolean = ???
}

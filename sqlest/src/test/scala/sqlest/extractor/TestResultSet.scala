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

import sqlest.ast._

case class TestResultSet(columns: Seq[AliasedColumn[_]])(rows: Seq[Any]*) extends AbstractResultSet {
  private var currentRow = -1
  private var lastCallWasNull = false

  private def get[A](columnName: String, orElse: => A) = {
    val columnIndex = columns.zipWithIndex
      .find(pair => pair._1.columnAlias == columnName)
      .map(_._2)
      .getOrElse(throw new Exception(s"Could not find $columnName in $columns"))

    try {
      lastCallWasNull = false
      val ans = rows(currentRow)(columnIndex).asInstanceOf[A]
      lastCallWasNull = ans == null
      ans
    } catch {
      case exn: ClassCastException =>
        lastCallWasNull = true
        orElse
    }
  }

  override def getInt(columnName: String) =
    get[Int](columnName, 0)

  override def getLong(columnName: String) =
    get[Long](columnName, 0L)

  override def getDouble(columnName: String) =
    get[Double](columnName, 0.0)

  override def getBigDecimal(columnName: String) =
    get[java.math.BigDecimal](columnName, null)

  override def getBoolean(columnName: String) =
    get[Boolean](columnName, false)

  override def getString(columnName: String) =
    get[String](columnName, null)

  override def getBytes(columnName: String) =
    get[Array[Byte]](columnName, null)

  override def getDate(columnName: String) =
    get[java.sql.Date](columnName, null)

  override def next() = {
    currentRow += 1
    currentRow < rows.length
  }

  override def toString = s"TestResultSet(${currentRow} of ${rows.length})"

  override def wasNull = lastCallWasNull
}
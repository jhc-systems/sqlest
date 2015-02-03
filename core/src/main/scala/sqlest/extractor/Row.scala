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

import java.sql.ResultSet
import org.joda.time.DateTime

trait Row {
  def getBoolean(columnName: String): Option[Boolean]
  def getInt(columnName: String): Option[Int]
  def getLong(columnName: String): Option[Long]
  def getDouble(columnName: String): Option[Double]
  def getBigDecimal(columnName: String): Option[BigDecimal]
  def getString(columnName: String): Option[String]
  def getDateTime(columnName: String): Option[DateTime]
}

object Row {
  implicit class ResultSetRowIterator(resultSet: ResultSet) extends Iterator[Row] {
    private var readNextRow = false
    private var hasNextRow = false
    val resultSetRow = ResultSetRow(resultSet)

    def next = {
      if (!readNextRow) resultSet.next
      readNextRow = false
      resultSetRow
    }

    def hasNext = {
      if (!readNextRow) {
        hasNextRow = resultSet.next
        readNextRow = true
      }
      hasNextRow
    }
  }

  case class ResultSetRow(resultSet: ResultSet) extends Row {
    def checkNull[A](value: A): Option[A] =
      if (!resultSet.wasNull) Some(value)
      else None

    def getBoolean(columnName: String) = checkNull(resultSet.getBoolean(columnName))
    def getInt(columnName: String) = checkNull(resultSet.getInt(columnName))
    def getLong(columnName: String) = checkNull(resultSet.getLong(columnName))
    def getDouble(columnName: String) = checkNull(resultSet.getDouble(columnName))
    def getBigDecimal(columnName: String) = Option(resultSet.getBigDecimal(columnName)).map(BigDecimal.apply)
    def getString(columnName: String) = checkNull(resultSet.getString(columnName))
    def getDateTime(columnName: String) = checkNull(new DateTime(resultSet.getDate(columnName)))
  }
}
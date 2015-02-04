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
import scala.reflect.runtime.universe._

trait Row {
  def getValue[A: TypeTag](columnName: String): Option[A]
}

object Row {
  implicit class ResultSetRowIterator(resultSet: ResultSet) extends Iterator[Row] {
    private var readNextRow = false
    private var hasNextRow = false
    private val resultSetRow = ResultSetRow(resultSet)

    def next: Row = {
      if (!readNextRow) resultSet.next
      readNextRow = false
      resultSetRow
    }

    def hasNext: Boolean = {
      if (!readNextRow) {
        hasNextRow = resultSet.next
        readNextRow = true
      }
      hasNextRow
    }
  }

  case class ResultSetRow(resultSet: ResultSet) extends Row {
    def getValue[A: TypeTag](columnName: String): Option[A] = (
      typeOf[A] match {
        case t if t =:= typeOf[Boolean] => checkNull(resultSet.getBoolean(columnName))
        case t if t =:= typeOf[Int] => checkNull(resultSet.getInt(columnName))
        case t if t =:= typeOf[Long] => checkNull(resultSet.getLong(columnName))
        case t if t =:= typeOf[Double] => checkNull(resultSet.getDouble(columnName))
        case t if t =:= typeOf[BigDecimal] => Option(resultSet.getBigDecimal(columnName)).map(BigDecimal.apply)
        case t if t =:= typeOf[String] => checkNull(resultSet.getString(columnName))
        case t if t =:= typeOf[DateTime] => checkNull(new DateTime(resultSet.getDate(columnName)))
      }
    ).asInstanceOf[Option[A]]

    private def checkNull[A](value: A): Option[A] =
      if (!resultSet.wasNull) Some(value)
      else None
  }
}
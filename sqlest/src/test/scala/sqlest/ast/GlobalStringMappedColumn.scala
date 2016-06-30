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

package sqlest.ast

trait GlobalStringMappedColumn {
  implicit val globalStringMappedColumn: ColumnType.Aux[String, String]
}
trait NoGlobalStringMappedColumn extends GlobalStringMappedColumn {
  implicit val globalStringMappedColumn: ColumnType.Aux[String, String] = StringColumnType
}
trait TrimmedGlobalStringMappedColumn extends GlobalStringMappedColumn {
  implicit val globalStringMappedColumn: ColumnType.Aux[String, String] = GlobalStringMappedColumn.UpperCaseTrimmedStringColumnType
}

object GlobalStringMappedColumn {
  object UpperCaseTrimmedStringColumnType extends MappedColumnType[String, String]()(StringColumnType) {
    val baseColumnType = StringColumnType
    def mappedRead(database: String) = rightTrim(database).toUpperCase
    def mappedWrite(value: String) = value

    private def rightTrim(s: String): String = {
      var i = s.length - 1
      while (i >= 0 && Character.isWhitespace(s.charAt(i))) i = i - 1
      s.substring(0, i + 1)
    }
  }
}


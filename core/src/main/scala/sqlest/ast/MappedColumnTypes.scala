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

import org.joda.time.DateTime

/** Standard set of MappedColumnTypes for various column types: */
trait MappedColumnTypes
    extends StringMappedColumnTypes
    with BooleanMappedColumnTypes
    with EnumerationMappedColumnTypes
    with NumericMappedColumnTypes
    with DateTimeMappedColumnTypes {

  // Reprovide the MappedColumnType type to custom Sqlest builds that use this trait:
  type MappedColumnType[ValueType, DatabaseType] = sqlest.ast.MappedColumnType[ValueType, DatabaseType]
  val MappedColumnType = sqlest.ast.MappedColumnType
}

trait StringMappedColumnTypes {
  case object TrimmedStringColumnType extends MappedColumnType[String, String] {
    val baseType = StringColumnType

    def read(database: String) = database.trim

    def write(value: String) = value
  }

  case object BlankIsNoneStringColumnType extends MappedColumnType[Option[String], String] {
    val baseType = StringColumnType

    def read(database: String) =
      if (database.trim != "") Some(database) else None

    def write(value: Option[String]) =
      value getOrElse ""
  }

}

trait BooleanMappedColumnTypes {
  case class MappedBooleanColumnType[DatabaseType](trueValue: DatabaseType, falseValue: DatabaseType)(implicit base: ColumnType[DatabaseType]) extends MappedColumnType[Boolean, DatabaseType] {
    val baseType = base

    def read(database: DatabaseType) = database == trueValue
    def write(value: Boolean) = if (value) trueValue else falseValue
  }

  val BooleanYNColumnType = MappedBooleanColumnType("Y", "N")
}

trait EnumerationMappedColumnTypes {
  case class EnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit base: ColumnType[DatabaseType]) extends MappedColumnType[ValueType, DatabaseType] {
    val baseType = base

    lazy val toDatabaseMappings = mappings.toMap
    lazy val toValueMappings = mappings.map { case (value, database) => (database, value) }.toMap

    def read(database: DatabaseType) = toValueMappings(database)
    def write(value: ValueType) = toDatabaseMappings(value)
  }

  case class OrderedEnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit base: ColumnType[DatabaseType]) extends MappedColumnType[ValueType, DatabaseType] with OrderedColumnType[ValueType] {
    val baseType = base

    lazy val toDatabaseMappings = mappings.toMap
    lazy val toValueMappings = mappings.map { case (value, database) => (database, value) }.toMap

    def read(database: DatabaseType) = toValueMappings(database)
    def write(value: ValueType) = toDatabaseMappings(value)

    def orderColumn(column: Column[ValueType]) = {
      val caseMappings =
        mappings
          .zipWithIndex
          .map { case ((value, _), index) => (ConstantColumn(value)(this), ConstantColumn(index)) }
          .toList

      CaseColumnColumn(column, caseMappings)
    }
  }
}

trait NumericMappedColumnTypes {
  case class ZeroIsNoneColumnType[A]()(implicit numeric: Numeric[A], base: ColumnType[A]) extends MappedColumnType[Option[A], A] {
    val baseType = base

    def read(database: A) =
      if (database != numeric.zero) Some(database) else None

    def write(value: Option[A]) =
      value getOrElse numeric.zero
  }

  case object BigDecimalStringColumnType extends MappedColumnType[BigDecimal, String] {
    val baseType = StringColumnType

    def read(database: String) = {
      val trimmed = database.trim
      if (trimmed != "") {
        if (trimmed.indexOf("/") == -1) {
          BigDecimal(trimmed)
        } else {
          BigDecimal(0)
        }

      } else BigDecimal(0)
    }

    def write(value: BigDecimal) = value.toString

  }

}

trait DateTimeMappedColumnTypes {
  case object YyyyMmDdColumnType extends MappedColumnType[DateTime, Int] {
    val baseType = IntColumnType

    def read(database: Int) = {
      val year = database / 10000
      val month = (database % 10000) / 100
      val day = database % 100

      new DateTime(year, month, day, 0, 0)
    }

    def write(value: DateTime) =
      value.getYear * 10000 + value.getMonthOfYear * 100 + value.getDayOfMonth
  }

  case object ZeroIsNoneYyyyMmDdColumnType extends MappedColumnType[Option[DateTime], Int] {
    val baseType = IntColumnType

    def read(database: Int) =
      if (database == 0) None
      else Some(YyyyMmDdColumnType.read(database))

    def write(value: Option[DateTime]) = value match {
      case Some(value) => value.getYear * 10000 + value.getMonthOfYear * 100 + value.getDayOfMonth
      case None => 0
    }
  }
}
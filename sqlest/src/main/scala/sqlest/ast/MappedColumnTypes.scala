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
    with DateTimeMappedColumnTypes
    with OptionColumnTypes {

  // Expose the ColumnType types to custom Sqlest builds that use this trait:
  val ColumnType = sqlest.ast.ColumnType
  val OptionColumnType = sqlest.ast.OptionColumnType
  type MappedColumnType[ValueType, DatabaseType] = sqlest.ast.MappedColumnType[ValueType, DatabaseType]
  val MappedColumnType = sqlest.ast.MappedColumnType
}

trait StringMappedColumnTypes {
  case object TrimmedStringColumnType extends MappedColumnType[String, String] {
    val baseColumnType = StringColumnType
    def read(database: Option[String]) = database.map(_.trim)
    def write(value: String) = value
  }
}

trait BooleanMappedColumnTypes {
  case class MappedBooleanColumnType[DatabaseType](trueValue: DatabaseType, falseValue: DatabaseType)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends MappedColumnType[Boolean, DatabaseType] {
    def read(database: Option[DatabaseType]) = database.map(_ == trueValue)
    def write(value: Boolean) = if (value) trueValue else falseValue
  }

  val BooleanYNColumnType = MappedBooleanColumnType("Y", "N")
}

trait EnumerationMappedColumnTypes {
  trait BaseEnumerationColumnType[ValueType, DatabaseType] extends MappedColumnType[ValueType, DatabaseType] {
    val mappings: Seq[(ValueType, DatabaseType)]
    val toDatabaseMappings = mappings.toMap
    val toValueMappings = mappings.map { case (value, database) => (database, value) }.toMap

    def read(database: Option[DatabaseType]) =
      database.map { database =>
        toValueMappings
          .get(database)
          .getOrElse(throw new NoSuchElementException(s"Could not read database value $database in EnumerationColumn"))
      }

    def write(value: ValueType) =
      toDatabaseMappings
        .get(value)
        .getOrElse(throw new NoSuchElementException(s"Could not write $value in EnumerationColumn"))
  }

  case class EnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends BaseEnumerationColumnType[ValueType, DatabaseType]

  case class OrderedEnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends BaseEnumerationColumnType[ValueType, DatabaseType] with OrderedColumnType {
    def orderColumn(column: Column[_]) = {
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
  case object BigDecimalStringColumnType extends MappedColumnType[BigDecimal, String] {
    val baseColumnType = StringColumnType

    def read(database: Option[String]) = {
      database.map { database: String =>
        val trimmed = database.trim
        if (trimmed != "") {
          if (trimmed.indexOf("/") == -1) {
            BigDecimal(trimmed)
          } else {
            BigDecimal(0)
          }
        } else BigDecimal(0)
      }
    }

    def write(value: BigDecimal) = value.toString
  }
}

trait DateTimeMappedColumnTypes {
  case object YyyyMmDdColumnType extends MappedColumnType[DateTime, Int] {
    val baseColumnType = IntColumnType

    def read(database: Option[Int]) = database.map { database =>
      val year = database / 10000
      val month = (database % 10000) / 100
      val day = database % 100

      new DateTime(year, month, day, 0, 0)
    }

    def write(value: DateTime) =
      value.getYear * 10000 + value.getMonthOfYear * 100 + value.getDayOfMonth
  }
}

trait OptionColumnTypes {
  def BlankIsNoneColumnType[A](implicit columnType: ColumnType.Aux[A, String]) =
    OptionColumnType[A, String]("", (_: String).trim == "")(columnType)

  def ZeroIsNoneColumnType[A, B: Numeric](columnType: ColumnType.Aux[A, B]) =
    OptionColumnType[A, B](implicitly[Numeric[B]].zero)(columnType)

  def ZeroIsNoneColumnType[A, B: Numeric](implicit columnType: ColumnType.Aux[A, B]) =
    OptionColumnType[A, B](implicitly[Numeric[B]].zero)(columnType)
}

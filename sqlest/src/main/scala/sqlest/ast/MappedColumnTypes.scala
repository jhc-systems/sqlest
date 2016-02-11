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

import org.joda.time.{ DateTime, LocalDate }

/** Standard set of MappedColumnTypes for various column types: */
trait MappedColumnTypes
    extends StringMappedColumnTypes
    with BooleanMappedColumnTypes
    with EnumerationMappedColumnTypes
    with NumericMappedColumnTypes
    with LocalDateMappedColumnTypes
    with OptionColumnTypes {

  // Expose the ColumnType types to custom Sqlest builds that use this trait:
  val ColumnType = sqlest.ast.ColumnType
  val OptionColumnType = sqlest.ast.OptionColumnType
  type MappedColumnType[ValueType, DatabaseType] = sqlest.ast.MappedColumnType[ValueType, DatabaseType]
  val MappedColumnType = sqlest.ast.MappedColumnType
}

trait StringMappedColumnTypes {
  /* Right trims any strings returned from the database */
  case object TrimmedStringColumnType extends MappedColumnType[String, String] {
    val baseColumnType = StringColumnType
    def read(database: Option[String]) = database.map(rightTrim)
    def write(value: String) = value

    private def rightTrim(s: String): String = {
      var i = s.length - 1
      while (i >= 0 && Character.isWhitespace(s.charAt(i))) i = i - 1
      s.substring(0, i + 1)
    }
  }
}

trait BooleanMappedColumnTypes {
  case class MappedBooleanColumnType[DatabaseType](trueValue: DatabaseType, falseValue: DatabaseType)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends MappedColumnType[Boolean, DatabaseType] {
    def read(database: Option[DatabaseType]) = database.map(_ == trueValue)
    def write(value: Boolean) = if (value) trueValue else falseValue
  }

  val BooleanYNColumnType = MappedBooleanColumnType("Y", "N")
  val Boolean10ColumnType = MappedBooleanColumnType(1, 0)
}

trait EnumerationMappedColumnTypes {
  trait BaseEnumerationColumnType[ValueType, DatabaseType] extends MappedColumnType[ValueType, DatabaseType] { self =>
    type WithDefault <: BaseEnumerationColumnType[ValueType, DatabaseType]

    val mappings: Seq[(ValueType, DatabaseType)]
    val default: Option[ValueType]
    val toDatabaseMappings = mappings.toMap
    val toValueMappings = mappings.map { case (value, database) => (database, value) }.toMap

    def read(database: Option[DatabaseType]) =
      database.map { database =>
        toValueMappings
          .get(database)
          .orElse(default)
          .getOrElse(throw new NoSuchElementException(s"Could not read database value $database in EnumerationColumn"))
      }

    def write(value: ValueType) =
      toDatabaseMappings
        .get(value)
        .getOrElse(throw new NoSuchElementException(s"Could not write $value in EnumerationColumn"))

    def withDefault(defaultValue: ValueType): WithDefault
  }

  case class EnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends BaseEnumerationColumnType[ValueType, DatabaseType] {
    type WithDefault = EnumerationColumnType[ValueType, DatabaseType]

    val default: Option[ValueType] = None

    def withDefault(defaultValue: ValueType) = new EnumerationColumnType(mappings: _*) {
      override val default = Some(defaultValue)
    }
  }

  case class OrderedEnumerationColumnType[ValueType, DatabaseType](mappings: (ValueType, DatabaseType)*)(implicit val baseColumnType: BaseColumnType[DatabaseType]) extends BaseEnumerationColumnType[ValueType, DatabaseType] with OrderedColumnType {
    type WithDefault = OrderedEnumerationColumnType[ValueType, DatabaseType]

    val default: Option[ValueType] = None

    def orderColumn(column: Column[_]) = {
      val caseMappings =
        mappings
          .zipWithIndex
          .map { case ((value, _), index) => (ConstantColumn(value)(this), ConstantColumn(index)) }
          .toList

      CaseColumnColumn(column, caseMappings)
    }

    def withDefault(defaultValue: ValueType) = new OrderedEnumerationColumnType(mappings: _*) {
      override val default = Some(defaultValue)
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

trait LocalDateMappedColumnTypes {
  case object YyyyMmDdColumnType extends MappedColumnType[LocalDate, Int] {
    val baseColumnType = IntColumnType

    def read(database: Option[Int]) = database.map { database =>
      val year = database / 10000
      val month = (database % 10000) / 100
      val day = database % 100

      new LocalDate(year, month, day)
    }

    def write(value: LocalDate) =
      value.getYear * 10000 + value.getMonthOfYear * 100 + value.getDayOfMonth
  }

  case object LocalDateFromDateTimeColumnType extends MappedColumnType[LocalDate, DateTime] {
    val baseColumnType = DateTimeColumnType
    def read(database: Option[DateTime]) = database.map(_.toLocalDate)
    def write(value: LocalDate) = value.toDateTimeAtStartOfDay
  }

  case object DateTimeFromLocalDateColumnType extends MappedColumnType[DateTime, LocalDate] {
    val baseColumnType = LocalDateColumnType
    def read(database: Option[LocalDate]) = database.map(_.toDateTimeAtStartOfDay)
    def write(value: DateTime) = value.toLocalDate
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

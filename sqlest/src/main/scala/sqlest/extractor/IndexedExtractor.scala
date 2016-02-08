package sqlest.extractor

import sqlest.ast._
import java.sql.ResultSet
import org.joda.time.{ DateTime, LocalDate }

/**
 * An extractor that has an index associated with it.
 *
 * This extractor is only for internal use when accessing generated keys
 *
 */
case class IndexedExtractor[A](index: Int)(implicit val columnType: ColumnType[A]) extends CellExtractor[ResultSet, A] {

  def read(resultSet: ResultSet) =
    columnType match {
      case baseColumnType: BaseColumnType[A] => readBaseType(resultSet, baseColumnType)
      case optionColumnType: OptionColumnType[_, _] => optionColumnType.read(readBaseType(resultSet, optionColumnType.baseColumnType)).asInstanceOf[Option[A]]
      case mappedColumnType: MappedColumnType[_, _] => mappedColumnType.read(readBaseType(resultSet, mappedColumnType.baseColumnType))
    }

  def readBaseType[B](resultSet: ResultSet, columnType: BaseColumnType[B]): Option[B] = {
    def checkNull[A](value: A): Option[A] =
      if (!resultSet.wasNull) Some(value)
      else None

    columnType match {
      case IntColumnType => checkNull(resultSet.getInt(index))
      case LongColumnType => checkNull(resultSet.getLong(index))
      case DoubleColumnType => checkNull(resultSet.getDouble(index))
      case BigDecimalColumnType => Option(resultSet.getBigDecimal(index)).map(BigDecimal.apply)
      case BooleanColumnType => checkNull(resultSet.getBoolean(index))
      case StringColumnType => checkNull(resultSet.getString(index))
      case DateTimeColumnType => checkNull(new DateTime(resultSet.getTimestamp(index)))
      case LocalDateColumnType => checkNull(new LocalDate(resultSet.getDate(index)))
      case ByteArrayColumnType => checkNull(resultSet.getBytes(index))
    }
  }

}
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

package sqlest

import java.sql.ResultSet
import org.joda.time.DateTime
import sqlest.extractor.TestResultSet
import sqlest.executor.ResultSetIterable

object TestData {
  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[String]("col2")(TrimmedStringColumnType)
    def columns = List(col1, col2)
  }
  object TableOne extends TableOne(None)

  class TableTwo(alias: Option[String]) extends Table("two", alias) {
    val col2 = column[String]("col2")
    val col3 = column[Int]("col3")
    def columns = List(col2, col3)
  }
  object TableTwo extends TableTwo(None)

  class TableThree(alias: Option[String]) extends Table("three", alias) {
    val col3 = column[Option[Int]]("col3")
    val col4 = column[Option[String]]("col4")
    def columns = List(col3, col4)
  }
  object TableThree extends TableThree(None)

  class TableFour(alias: Option[String]) extends Table("four", alias) {
    val mapped = column[Boolean]("mapped")(BooleanYNColumnType)
    def columns = List(mapped)
  }
  object TableFour extends TableFour(None)

  class TableFive(alias: Option[String]) extends Table("five", alias) {
    val intCol = column[Int]("intCol")
    val longCol = column[Long]("longCol")
    val doubleCol = column[Double]("doubleCol")
    val bigDecimalCol = column[BigDecimal]("bigDecimalCol")
    val booleanColumn = column[Boolean]("booleanColumn")
    val stringColumn = column[String]("stringColumn")
    val dateTimeCol = column[DateTime]("dateTimeCol")
    val byteArrayCol = column[Array[Byte]]("byteArrayCol")

    def columns = List(intCol, longCol, doubleCol, bigDecimalCol, booleanColumn, stringColumn, dateTimeCol, byteArrayCol)
  }
  object TableFive extends TableFive(None)

  case class WrappedString(inner: String)
  case class WrappedInt(inner: Int)
  class TableSix(alias: Option[String]) extends Table("six", alias) {
    val trimmedString = column[Option[WrappedString]]("trimmedString")(BlankIsNoneColumnType(ColumnType[WrappedString, String].compose(TrimmedStringColumnType)))
    val zeroIsNoneWrappedInt = column[Option[WrappedInt]]("zeroIsNoneWrappedInt")(ZeroIsNoneColumnType[WrappedInt, Int])
    val zeroIsNoneDateTime = column[Option[DateTime]]("zeroIsNoneDateTime")(ZeroIsNoneColumnType(YyyyMmDdColumnType))
    def columns = List(trimmedString, zeroIsNoneWrappedInt, zeroIsNoneDateTime)
  }
  object TableSix extends TableSix(None)

  case class One(a: Int, b: String)
  case class Two(a: String, b: Int)
  case class Three(a: Option[Int], b: Option[String])

  case class AggregateOneTwo(one: One, two: Two)
  case class AggregateOneTwoThree(one: One, two: Two, three: Three)
  case class AggregateOneTwoOptionThree(one: One, two: Two, three: Option[Three])
  case class AggregateOneTwoThenThree(oneTwo: AggregateOneTwo, three: Three)

  def testResultSet = {
    TestResultSet(TableOne.columns ++ TableTwo.columns ++ TableThree.columns)(
      Seq(1, "a", "b", 2, null, "x"),
      Seq(3, "c", "d", 4, 9, null),
      Seq(-1, "e", "f", 6, null, null)
    )
  }

  implicit def resultSetIterable(resultSet: ResultSet) = ResultSetIterable(resultSet)
}
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
package extractor

import java.sql.ResultSet
import org.scalatest._
import org.scalatest.matchers._

/**
 * Proof-of-concept implementation of a table, domain class and extractor
 * with more than 22 fields in them. It's not pretty but it works.
 */
class BigTableSpec extends FlatSpec with Matchers {
  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[Int]("col2")
    val col3 = column[Int]("col3")
    val col4 = column[Int]("col4")
    val col5 = column[Int]("col5")
    val col6 = column[Int]("col6")
    val col7 = column[Int]("col7")
    val col8 = column[Int]("col8")
    val col9 = column[Int]("col9")
    val col10 = column[Int]("col10")
    val col11 = column[Int]("col11")
    val col12 = column[Int]("col12")
    val col13 = column[Int]("col13")
    val col14 = column[Int]("col14")
    val col15 = column[Int]("col15")
    val col16 = column[Int]("col16")
    val col17 = column[Int]("col17")
    val col18 = column[Int]("col18")
    val col19 = column[Int]("col19")
    val col20 = column[Int]("col20")
    val col21 = column[Int]("col21")
    val col22 = column[Int]("col22")
    val col23 = column[Int]("col23")
    val col24 = column[Int]("col24")
    val col25 = column[Int]("col25")
    val col26 = column[Int]("col26")
    val col27 = column[Int]("col27")
    val col28 = column[Int]("col28")
    val col29 = column[Int]("col29")
    val col30 = column[Int]("col30")
    def as(alias: String) = new TableOne(Some(alias))
  }

  object TableOne extends TableOne(None)

  case class DomainClass(
    val col1: Int,
    val col2: Int,
    val col3: Int,
    val col4: Int,
    val col5: Int,
    val col6: Int,
    val col7: Int,
    val col8: Int,
    val col9: Int,
    val col10: Int,
    val col11: Int,
    val col12: Int,
    val col13: Int,
    val col14: Int,
    val col15: Int,
    val col16: Int,
    val col17: Int,
    val col18: Int,
    val col19: Int,
    val col20: Int,
    val col21: Int,
    val col22: Int,
    val col23: Int,
    val col24: Int,
    val col25: Int,
    val col26: Int,
    val col27: Int,
    val col28: Int,
    val col29: Int,
    val col30: Int)

  object DomainClassExtractor extends SingleExtractor[DomainClass] {
    import TableOne._

    type Accumulator = DomainClass
    val columns = List(col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17, col18, col19, col20, col21, col22, col23, col24, col25, col26, col27, col28, col29, col30)

    def initialize(row: ResultSet) = read(row)

    def accumulate(row: ResultSet, accumulator: DomainClass) = read(row)

    def emit(accumulator: DomainClass) = Some(accumulator)

    def read(row: ResultSet) = new DomainClass(
      col1.emit(col1.initialize(row)).get,
      col2.emit(col2.initialize(row)).get,
      col3.emit(col3.initialize(row)).get,
      col4.emit(col4.initialize(row)).get,
      col5.emit(col5.initialize(row)).get,
      col6.emit(col6.initialize(row)).get,
      col7.emit(col7.initialize(row)).get,
      col8.emit(col8.initialize(row)).get,
      col9.emit(col9.initialize(row)).get,
      col10.emit(col10.initialize(row)).get,
      col11.emit(col11.initialize(row)).get,
      col12.emit(col12.initialize(row)).get,
      col13.emit(col13.initialize(row)).get,
      col14.emit(col14.initialize(row)).get,
      col15.emit(col15.initialize(row)).get,
      col16.emit(col16.initialize(row)).get,
      col17.emit(col17.initialize(row)).get,
      col18.emit(col18.initialize(row)).get,
      col19.emit(col19.initialize(row)).get,
      col20.emit(col20.initialize(row)).get,
      col21.emit(col21.initialize(row)).get,
      col22.emit(col22.initialize(row)).get,
      col23.emit(col23.initialize(row)).get,
      col24.emit(col24.initialize(row)).get,
      col25.emit(col25.initialize(row)).get,
      col26.emit(col26.initialize(row)).get,
      col27.emit(col27.initialize(row)).get,
      col28.emit(col28.initialize(row)).get,
      col29.emit(col29.initialize(row)).get,
      col30.emit(col30.initialize(row)).get
    )
  }

}

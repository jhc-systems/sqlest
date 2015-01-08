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

package sqlest.untyped.ast

import org.joda.time._
import org.scalatest._
import org.scalatest.matchers._
import sqlest._

class ColumnSpec extends FlatSpec with Matchers {

  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val intCol = column[Int]("intCol")
    val longCol = column[Long]("Col")
    val doubleCol = column[Double]("Col")
    val bigDecimalCol = column[BigDecimal]("Col")
    val booleanCol = column[Boolean]("Col")
    val stringCol = column[String]("Col")
    val dateTimeCol = column[DateTime]("Col")
    val mappedCol = column[Int]("Col", MappedColumnType[Int, String](_.toInt, _.toString))
  }

  object TableOne extends TableOne(None)

  "untypedEq" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedEq "1") should equal(Some(TableOne.intCol === 1))
    (TableOne.longCol untypedEq "1") should equal(Some(TableOne.longCol === 1))
    (TableOne.doubleCol untypedEq "1") should equal(Some(TableOne.doubleCol === 1))
    (TableOne.bigDecimalCol untypedEq "1") should equal(Some(TableOne.bigDecimalCol === 1))
    (TableOne.booleanCol untypedEq "true") should equal(Some(TableOne.booleanCol === true))
    (TableOne.stringCol untypedEq "abc") should equal(Some(TableOne.stringCol === "abc"))
    (TableOne.dateTimeCol untypedEq "2014-01-01T09:00:00.000Z") should equal(Some(TableOne.dateTimeCol === new DateTime(2014, 1, 1, 9, 0, 0, 0)))
    // TODO
    // (TableOne.mappedCol untypedEq "2") should equal(Some(TableOne.mappedCol === 2))
  }

  it should "handle comparison with an invalid string" in {
    (TableOne.intCol untypedEq "a") should equal(None)
    (TableOne.longCol untypedEq "a") should equal(None)
    (TableOne.doubleCol untypedEq "a") should equal(None)
    (TableOne.bigDecimalCol untypedEq "a") should equal(None)
    (TableOne.booleanCol untypedEq "a") should equal(None)
    (TableOne.dateTimeCol untypedEq "a") should equal(None)
    // TODO
    // (TableOne.mappedCol untypedEq "a") should equal(None)
  }

  "untypedNe" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedNe "1") should equal(Some(TableOne.intCol =!= 1))
  }

  "untypedLt" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedLt "1") should equal(Some(TableOne.intCol < 1))
  }

  "untypedGt" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedGt "1") should equal(Some(TableOne.intCol > 1))
  }

  "untypedLte" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedLte "1") should equal(Some(TableOne.intCol <= 1))
  }

  "untypedGte" should "allow comparison with a valid string" in {
    (TableOne.intCol untypedGte "1") should equal(Some(TableOne.intCol >= 1))
  }

  "untypedIn" should "allow comparison with a valid list of strings" in {
    (TableOne.intCol untypedIn List("A", "2", "3")) should equal(None)
    (TableOne.intCol untypedIn List("1", "2", "3")) should equal(Some(TableOne.intCol in (1, 2, 3)))
  }

  "untypedStartsWith" should "allow comparison with a string" in {
    (TableOne.stringCol untypedStartsWith "a%b_c^") should equal(Some(TableOne.stringCol like "a^%b^_c^^%"))
  }

  it should "ignore non-string columns" in {
    (TableOne.intCol untypedStartsWith "1") should equal(None)
    (TableOne.longCol untypedStartsWith "1") should equal(None)
    (TableOne.doubleCol untypedStartsWith "1") should equal(None)
    (TableOne.bigDecimalCol untypedStartsWith "1") should equal(None)
    (TableOne.booleanCol untypedStartsWith "true") should equal(None)
    (TableOne.dateTimeCol untypedStartsWith "2014-01-01T09:00:00.000Z") should equal(None)
  }

  "untypedContains" should "allow comparison with a string" in {
    (TableOne.stringCol untypedContains "a%b_c^") should equal(Some(TableOne.stringCol like "%a^%b^_c^^%"))
  }

  it should "ignore non-string columns" in {
    (TableOne.intCol untypedContains "1") should equal(None)
    (TableOne.longCol untypedContains "1") should equal(None)
    (TableOne.doubleCol untypedContains "1") should equal(None)
    (TableOne.bigDecimalCol untypedContains "1") should equal(None)
    (TableOne.booleanCol untypedContains "true") should equal(None)
    (TableOne.dateTimeCol untypedContains "2014-01-01T09:00:00.000Z") should equal(None)
  }
}
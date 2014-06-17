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

import sqlest.extractor.TestResultSet

object TestData {
  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[String]("col2")

    def as(alias: String) = new TableOne(Some(alias))
  }

  object TableOne extends TableOne(None)

  class TableTwo(alias: Option[String]) extends Table("two", alias) {
    val col2 = column[String]("col2")
    val col3 = column[Int]("col3")

    def as(alias: String) = new TableTwo(Some(alias))
  }

  object TableTwo extends TableTwo(None)

  class TableThree(alias: Option[String]) extends Table("three", alias) {
    val col3 = column[Option[Int]]("col3")
    val col4 = column[Option[String]]("col4")

    def as(alias: String) = new TableThree(Some(alias))
  }

  object TableThree extends TableThree(None)

  class TableFour(alias: Option[String]) extends Table("four", alias) {
    val mapped = column[Boolean]("mapped", BooleanYNColumnType)
  }

  object TableFour extends TableFour(None)

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
}
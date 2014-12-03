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

package sqlest.untyped

import sqlest._
import sqlest.extractor.TestResultSet
import org.scalatest._
import org.scalatest.matchers._

class FindColumnSpec extends FlatSpec with Matchers {
  import TestData._

  "column finder" should "find a column in a single extractor" in {
    val extractor = extractColumn(TableOne.col1)
    extractor.findColumn("") should equal(Some(TableOne.col1))
    extractor.findColumn("foo") should equal(None)
  }

  it should "find a column in a bare mapped extractor" in {
    val extractor = TableOne.col1.map(_ * 2)
    extractor.findColumn("") should equal(Some(TableOne.col1))
    extractor.findColumn("foo") should equal(None)
  }

  it should "find a column in a bare list extractor" in {
    val extractor = TableOne.col1.asList
    extractor.findColumn("") should equal(Some(TableOne.col1))
    extractor.findColumn("foo") should equal(None)
  }

  it should "find a column in a bare grouped extractor" in {
    val extractor = TableOne.col1.groupBy(TableOne.col1)
    extractor.findColumn("") should equal(Some(TableOne.col1))
    extractor.findColumn("foo") should equal(None)
  }

  it should "find a column in a bare product extractor" in {
    val extractor = extract(
      TableOne.col1,
      TableOne.col2
    )

    extractor.findColumn("0") should equal(Some(TableOne.col1))
    extractor.findColumn("1") should equal(Some(TableOne.col2))
    extractor.findColumn("2") should equal(None)
    extractor.findColumn("foo") should equal(None)
    extractor.findColumn("") should equal(None)
  }

  it should "NOT find a column in a mapped/product extractor" in {
    val extractor = extract(
      TableOne.col1,
      TableOne.col2
    ).map(One.tupled)

    extractor.findColumn("a") should equal(None)
    extractor.findColumn("b") should equal(None)
    extractor.findColumn("2") should equal(None)
    extractor.findColumn("foo") should equal(None)
    extractor.findColumn("") should equal(None)
  }

  it should "find a column in named/product extractor" in {
    val extractor = extract[One](
      a = TableOne.col1,
      b = TableOne.col2
    )

    extractor.findColumn("a") should equal(Some(TableOne.col1))
    extractor.findColumn("b") should equal(Some(TableOne.col2))
    extractor.findColumn("2") should equal(None)
    extractor.findColumn("foo") should equal(None)
    extractor.findColumn("") should equal(None)
  }

  it should "find a column in nested named extractors" in {
    val extractor = extract[AggregateOneTwoThenThree](
      oneTwo = extract[AggregateOneTwo](
        one = extract[One](
          a = TableOne.col1,
          b = TableOne.col2
        ),
        two = extract[Two](
          a = TableTwo.col2,
          b = TableTwo.col3
        )
      ),
      three = extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      )
    )

    extractor.findColumn("oneTwo.one.a") should equal(Some(TableOne.col1))
    extractor.findColumn("oneTwo.one.b") should equal(Some(TableOne.col2))
    extractor.findColumn("oneTwo.two.a") should equal(Some(TableTwo.col2))
    extractor.findColumn("oneTwo.two.b") should equal(Some(TableTwo.col3))
    extractor.findColumn("three.a") should equal(Some(TableThree.col3))
    extractor.findColumn("three.b") should equal(Some(TableThree.col4))
    extractor.findColumn("oneTwo.a") should equal(None)
    extractor.findColumn("one.a") should equal(None)
    extractor.findColumn("oneTwo") should equal(None)
    extractor.findColumn("one") should equal(None)
    extractor.findColumn("a") should equal(None)
    extractor.findColumn("") should equal(None)
  }

  // it should "find a column in a nested named and product extractor" in {
  //   case class Outer(oneTwo: (One, Two), three: Three)

  //   val extractor = extract[Outer](
  //     oneTwo = extract[Tuple2[One, Two]](
  //       one = extract[One](
  //         a = TableOne.col1,
  //         b = TableOne.col2
  //       ),
  //       two = extract[Two](
  //         a = TableTwo.col2,
  //         b = TableTwo.col3
  //       )
  //     ),
  //     three = extract[Three](
  //       a = TableThree.col3,
  //       b = TableThree.col4
  //     )
  //   )

  //   extractor.findColumn("oneTwo.0.a") should equal(Some(TableOne.col1))
  //   extractor.findColumn("oneTwo.0.b") should equal(Some(TableOne.col2))
  //   extractor.findColumn("oneTwo.1.a") should equal(Some(TableTwo.col2))
  //   extractor.findColumn("oneTwo.1.b") should equal(Some(TableTwo.col3))
  //   extractor.findColumn("three.a") should equal(Some(TableThree.col3))
  //   extractor.findColumn("three.b") should equal(Some(TableThree.col4))
  //   extractor.findColumn("oneTwo.a") should equal(None)
  //   extractor.findColumn("one.a") should equal(None)
  //   extractor.findColumn("oneTwo") should equal(None)
  //   extractor.findColumn("one") should equal(None)
  //   extractor.findColumn("a") should equal(None)
  //   extractor.findColumn("") should equal(None)
  // }
}
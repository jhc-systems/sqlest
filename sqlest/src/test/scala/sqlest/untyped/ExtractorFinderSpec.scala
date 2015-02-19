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

class ExtractorFinderSpec extends FlatSpec with Matchers {
  import TestData._

  "cell extractor finder" should "find a cell extractor in a single extractor" in {
    val extractor = TableOne.col1
    extractor.findCellExtractor("") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("foo") should equal(None)
  }

  it should "find a cell extractor in a bare mapped extractor" in {
    val extractor = TableOne.col1.map(_ * 2)
    extractor.findCellExtractor("") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("foo") should equal(None)
  }

  it should "find a cell extractor in a bare list extractor" in {
    val extractor = TableOne.col1.asList
    extractor.findCellExtractor("") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("foo") should equal(None)
  }

  it should "find a cell extractor in a bare grouped extractor" in {
    val extractor = TableOne.col1.groupBy(TableOne.col1)
    extractor.findCellExtractor("") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("foo") should equal(None)
  }

  it should "find a cell extractor in a bare product extractor" in {
    val extractor = extractTuple(
      TableOne.col1,
      TableOne.col2
    )

    extractor.findCellExtractor("0") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("1") should equal(Some(TableOne.col2))
    extractor.findCellExtractor("2") should equal(None)
    extractor.findCellExtractor("foo") should equal(None)
    extractor.findCellExtractor("") should equal(None)
  }

  it should "NOT find a cell extractor in a mapped/product extractor" in {
    val extractor = extractTuple(
      TableOne.col1,
      TableOne.col2
    ).map(One.tupled)

    extractor.findCellExtractor("a") should equal(None)
    extractor.findCellExtractor("b") should equal(None)
    extractor.findCellExtractor("2") should equal(None)
    extractor.findCellExtractor("foo") should equal(None)
    extractor.findCellExtractor("") should equal(None)
  }

  it should "find a cell extractor in named/product extractor" in {
    val extractor = extract[One](
      a = TableOne.col1,
      b = TableOne.col2
    )

    extractor.findCellExtractor("a") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("b") should equal(Some(TableOne.col2))
    extractor.findCellExtractor("2") should equal(None)
    extractor.findCellExtractor("foo") should equal(None)
    extractor.findCellExtractor("") should equal(None)
  }

  it should "find a cell extractor in nested named extractors" in {
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

    extractor.findCellExtractor("oneTwo.one.a") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("oneTwo.one.b") should equal(Some(TableOne.col2))
    extractor.findCellExtractor("oneTwo.two.a") should equal(Some(TableTwo.col2))
    extractor.findCellExtractor("oneTwo.two.b") should equal(Some(TableTwo.col3))
    extractor.findCellExtractor("three.a") should equal(Some(TableThree.col3))
    extractor.findCellExtractor("three.b") should equal(Some(TableThree.col4))
    extractor.findCellExtractor("oneTwo.a") should equal(None)
    extractor.findCellExtractor("one.a") should equal(None)
    extractor.findCellExtractor("oneTwo") should equal(None)
    extractor.findCellExtractor("one") should equal(None)
    extractor.findCellExtractor("a") should equal(None)
    extractor.findCellExtractor("") should equal(None)
  }

  case class Outer(oneTwo: (One, Two), three: Three)
  it should "find a cell extractor in a nested named and product extractor" in {

    val extractor = extract[Outer](
      oneTwo = extractTuple(
        extract[One](
          a = TableOne.col1,
          b = TableOne.col2
        ),
        extract[Two](
          a = TableTwo.col2,
          b = TableTwo.col3
        )
      ),
      three = extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      )
    )

    extractor.findCellExtractor("oneTwo.0.a") should equal(Some(TableOne.col1))
    extractor.findCellExtractor("oneTwo.0.b") should equal(Some(TableOne.col2))
    extractor.findCellExtractor("oneTwo.1.a") should equal(Some(TableTwo.col2))
    extractor.findCellExtractor("oneTwo.1.b") should equal(Some(TableTwo.col3))
    extractor.findCellExtractor("three.a") should equal(Some(TableThree.col3))
    extractor.findCellExtractor("three.b") should equal(Some(TableThree.col4))
    extractor.findCellExtractor("oneTwo.a") should equal(None)
    extractor.findCellExtractor("one.a") should equal(None)
    extractor.findCellExtractor("oneTwo") should equal(None)
    extractor.findCellExtractor("one") should equal(None)
    extractor.findCellExtractor("a") should equal(None)
    extractor.findCellExtractor("") should equal(None)
  }
}
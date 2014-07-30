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

package sqlest.untyped.extractor.syntax

import sqlest._
import sqlest.extractor.TestResultSet
import org.scalatest._
import org.scalatest.matchers._

// TODO: The NamedExtractSyntax macro has issues with path-dependent types.
// We don't use path dependent types in our production codebase, so we've
// decided not to address this for now. This test data feeds a pending
// test case below that illustrates the problem:
trait PathDependenceTestData {
  case class PathDependentOne(a: Int, b: String)
  case class PathDependentTwo(a: String, b: Int)
  case class PathDependentOneTwo(one: PathDependentOne, two: PathDependentTwo)
}

class NamedExtractSyntaxSpec extends FlatSpec with Matchers with PathDependenceTestData {
  import TestData._

  // TODO: We can't currently inherit from TestData here
  // because the macro can't reify TestData.this.One and
  // NamedExtractorSyntaxSpec.this.One. Fix this!

  case class Tiny(a: Int)

  val simpleExtractor = extractNamed[One](
    "a" -> TableOne.col1,
    "b" -> TableOne.col2
  )

  val nestedExtractor = extractNamed[AggregateOneTwo](
    "one" -> extractNamed[One](
      "a" -> TableOne.col1,
      "b" -> TableOne.col2
    ),
    "two" -> extractNamed[Two](
      "a" -> TableTwo.col2,
      "b" -> TableTwo.col3
    )
  )

  "simple extractNamed" should "have the correct extractOne behaviour" in {
    simpleExtractor.extractOne(testResultSet) should equal(Some(
      One(1, "a")
    ))
  }

  it should "have the correct extractAll behaviour" in {
    simpleExtractor.extractAll(testResultSet) should equal(List(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "support findColumn syntax" in {
    simpleExtractor.findColumn("a") should equal(Some(TableOne.col1))
    simpleExtractor.findColumn("b") should equal(Some(TableOne.col2))
    simpleExtractor.findColumn("c") should equal(None)
  }

  "nested extractNamed" should "have the correct extractOne behaviour" in {
    nestedExtractor.extractOne(testResultSet) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("b", 2))
    ))
  }

  it should "have the correct extractAll behaviour" in {
    nestedExtractor.extractAll(testResultSet) should equal(List(
      AggregateOneTwo(One(1, "a"), Two("b", 2)),
      AggregateOneTwo(One(3, "c"), Two("d", 4)),
      AggregateOneTwo(One(-1, "e"), Two("f", 6))
    ))
  }

  it should "support findColumn syntax" in {
    nestedExtractor.findColumn("one.a") should equal(Some(TableOne.col1))
    nestedExtractor.findColumn("two.b") should equal(Some(TableTwo.col3))
    nestedExtractor.findColumn("two.c") should equal(None)
  }

  "general namedExtract" should "work for case classes with one field" in {
    extractNamed[Tiny]("a" -> TableOne.col1)
  }

  it should "handle path-dependent types correctly" in {
    pending
    // TODO: This should compile, but doesn't due to a bug related to path dependent types:
    // extractNamed[PathDependentOneTwo](
    //   "one" -> extractNamed[PathDependentOne](
    //     "a" -> TableOne.col1,
    //     "b" -> TableOne.col2
    //   ),
    //   "two" -> extractNamed[PathDependentTwo](
    //     "a" -> TableTwo.col2,
    //     "b" -> TableTwo.col3
    //   )
    // )
  }

  // --------------------------

  // TODO: Implement this test with illTyped:
  it should "fail if there are too few arguments" in {
    pending
    // extractNamed[One](
    //   "a" -> TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail if there are too many arguments" in {
    pending
    // extractNamed[One](
    //   "a" -> TableOne.col1,
    //   "b" -> TableOne.col1,
    //   "c" -> TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong types of arguments" in {
    pending
    // extractNamed[One](
    //   "a" -> TableOne.col2,
    //   "b" -> TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong argument names" in {
    pending
    // extractNamed[One](
    //   "b" -> TableOne.col1,
    //   "a" -> TableOne.col2
    // )
  }

}
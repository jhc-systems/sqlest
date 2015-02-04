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

  val simpleExtractor = extract[One](
    a = TableOne.col1,
    b = TableOne.col2
  )

  val nestedExtractor = extract[AggregateOneTwo](
    one = extract[One](
      a = TableOne.col1,
      b = TableOne.col2
    ),
    two = extract[Two](
      a = TableTwo.col2,
      b = TableTwo.col3
    )
  )

  "simple extract" should "have the correct extractHeadOption behaviour" in {
    simpleExtractor.extractHeadOption(testResultSet) should equal(Some(
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

  it should "support findCellExtractor syntax" in {
    simpleExtractor.findCellExtractor("a") should equal(Some(extractColumn(TableOne.col1)))
    simpleExtractor.findCellExtractor("b") should equal(Some(extractColumn(TableOne.col2)))
    simpleExtractor.findCellExtractor("c") should equal(None)
  }

  "nested extract" should "have the correct extractHeadOption behaviour" in {
    nestedExtractor.extractHeadOption(testResultSet) should equal(Some(
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

  it should "support findCellExtractor syntax" in {
    nestedExtractor.findCellExtractor("one.a") should equal(Some(extractColumn(TableOne.col1)))
    nestedExtractor.findCellExtractor("two.b") should equal(Some(extractColumn(TableTwo.col3)))
    nestedExtractor.findCellExtractor("two.c") should equal(None)
  }

  "general namedExtract" should "work for case classes with one field" in {
    extract[Tiny](a = TableOne.col1)
  }

  class Multiple(a: Int, b: Int)
  object Multiple {
    def apply(a: Int) = new Multiple(a, 37)
    def apply(a: Int, b: Int) = new Multiple(a, b)
    def apply(a: Int, b: Int, c: Int) = new Multiple(a, b + c)
  }
  it should "work for classes with multiple apply methods" in {
    extract[Multiple](a = TableOne.col1)
    extract[Multiple](a = TableOne.col1, b = TableOne.col1)
    extract[Multiple](a = TableOne.col1, b = TableOne.col1, c = TableOne.col1)
  }

  case class DefaultParams(a: Int, b: String = "sweet")
  it should "work for apply methods with default parameters" in {
    extract[DefaultParams](a = TableOne.col1, b = TableOne.col2)
    extract[DefaultParams](a = TableOne.col1)
  }

  case class VarargsParams(a: Int, b: String*)
  it should "work for apply methods with varargs" in {
    extract[VarargsParams](TableOne.col1, TableOne.col2, TableOne.col2)
    extract[VarargsParams](TableOne.col1, TableOne.col2)
    extract[VarargsParams](TableOne.col1)
  }

  case class TypeParamClass[A, B](a: A, b: B)
  case class ReversedTypeParamClass[A, B](b: B, a: A)
  case class DuplicateTypeParamClass[A](a1: A, a2: A)
  case class MixedTypeParamClass[A](s: String, a: A)
  it should "work for apply methods with type parameters" in {
    extract[TypeParamClass[String, Int]](TableOne.col2, TableOne.col1)
    extract[TypeParamClass[String, Int]](TableOne.col2, extractConstant(6))
    extract[ReversedTypeParamClass[String, Int]](TableOne.col1, TableOne.col2)
    extract[DuplicateTypeParamClass[Int]](TableOne.col1, TableTwo.col3)
    extract[MixedTypeParamClass[Int]](TableTwo.col2, TableOne.col1)
    extract[List[String]](TableOne.col2, TableTwo.col2)
    extract[Map[Int, String]](TableOne.col1 -> TableOne.col2, TableTwo.col3 -> TableTwo.col2)
  }

  it should "handle path-dependent types correctly" in {
    pending
    // TODO: This should compile, but doesn't due to a bug related to path dependent types:
    // extract[PathDependentOneTwo](
    //   one = extract[PathDependentOne](
    //     a = TableOne.col1,
    //     b = TableOne.col2
    //   ),
    //   two = extract[PathDependentTwo](
    //     a = TableTwo.col2,
    //     b = TableTwo.col3
    //   )
    // )
  }

  // --------------------------

  // TODO: Implement this test with illTyped:
  it should "fail if there are too few arguments" in {
    pending
    // extract[One](
    //   a = TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail if there are too many arguments" in {
    pending
    // extract[One](
    //   a = TableOne.col1,
    //   b = TableOne.col1,
    //   c = TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong types of arguments" in {
    pending
    // extract[One](
    //   a = TableOne.col2,
    //   b = TableOne.col1
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong argument names" in {
    pending
    // extract[One](
    //   b = TableOne.col1,
    //   a = TableOne.col2
    // )
  }

}
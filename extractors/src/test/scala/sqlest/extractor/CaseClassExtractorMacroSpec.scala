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

package sqlest.extractor

import org.scalatest._
import org.scalatest.matchers._

// TODO: The CaseClassExtractorMacro macro has issues with path-dependent types.
// We don't use path dependent types in our production codebase, so we've
// decided not to address this for now. This test data feeds a pending
// test case below that illustrates the problem:
trait PathDependenceTestData {
  case class PathDependentOne(a: Int, b: String)
  case class PathDependentTwo(a: String, b: Int)
  case class PathDependentOneTwo(one: PathDependentOne, two: PathDependentTwo)
}

sealed trait Shape
case object Circle extends Shape
case object Tetrahedron extends Shape
case object Plane extends Shape

class CaseClassExtractorMacroSpec extends FlatSpec with Matchers with ExtractorSyntax[Tuple3[Shape, Int, String]] with PathDependenceTestData {

  // TODO: We can't currently inherit from TestData here
  // because the macro can't reify TestData.this.One and
  // NamedExtractorSyntaxSpec.this.One. Fix this!

  case class One(a: Int, b: String)
  case class Two(a: String, b: Shape)

  case class AggregateOneTwo(one: One, two: Two)

  case class Tiny(a: Int)

  case object ShapeExtractor extends CellExtractor[Tuple3[Shape, Int, String], Shape] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._1)
  }

  case object IntExtractor extends CellExtractor[Tuple3[Shape, Int, String], Int] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._2)
  }

  case object StringExtractor extends CellExtractor[Tuple3[Shape, Int, String], String] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._3)
  }

  val simpleExtractor = extract[One](
    a = IntExtractor,
    b = StringExtractor
  )

  val nestedExtractor = extract[AggregateOneTwo](
    one = extract[One](
      a = IntExtractor,
      b = StringExtractor
    ),
    two = extract[Two](
      a = StringExtractor,
      b = ShapeExtractor
    )
  )

  val tupleList = List(
    (Circle, 1, "a"),
    (Tetrahedron, 3, "c"),
    (Plane, -1, "e")
  )

  "extract[A]" should "have the correct extractHeadOption behaviour" in {
    simpleExtractor.extractHeadOption(tupleList) should equal(Some(
      One(1, "a")
    ))
  }

  it should "have the correct extractAll behaviour" in {
    simpleExtractor.extractAll(tupleList) should equal(List(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "support findCellExtractor syntax" in {
    simpleExtractor.findCellExtractor("a") should equal(Some(IntExtractor))
    simpleExtractor.findCellExtractor("b") should equal(Some(StringExtractor))
    simpleExtractor.findCellExtractor("c") should equal(None)
  }

  it should "work for case classes with one field" in {
    extract[Tiny](a = IntExtractor)
  }

  class Multiple(a: Int, b: Int)
  object Multiple {
    def apply(a: Int) = new Multiple(a, 37)
    def apply(a: Int, b: Int) = new Multiple(a, b)
    def apply(a: Int, b: Int, c: Int) = new Multiple(a, b + c)
  }
  it should "work for classes with multiple apply methods" in {
    extract[Multiple](a = IntExtractor)
    extract[Multiple](a = IntExtractor, b = IntExtractor)
    extract[Multiple](a = IntExtractor, b = IntExtractor, c = IntExtractor)
  }

  case class DefaultParams(a: Int, b: String = "sweet")
  it should "work for apply methods with default parameters" in {
    extract[DefaultParams](a = IntExtractor, b = StringExtractor)
    extract[DefaultParams](a = IntExtractor)
  }

  case class VarargsParams(a: Int, b: String*)
  it should "work for apply methods with varargs" in {
    extract[VarargsParams](IntExtractor, StringExtractor, StringExtractor)
    extract[VarargsParams](IntExtractor, StringExtractor)
    extract[VarargsParams](IntExtractor)
  }

  case class TypeParamClass[A, B](a: A, b: B)
  case class ReversedTypeParamClass[A, B](b: B, a: A)
  case class DuplicateTypeParamClass[A](a1: A, a2: A)
  case class MixedTypeParamClass[A](s: String, a: A)
  it should "work for apply methods with type parameters" in {
    extract[TypeParamClass[String, Int]](StringExtractor, IntExtractor)
    extract[TypeParamClass[String, Int]](StringExtractor, extractConstant(6))
    extract[ReversedTypeParamClass[String, Int]](IntExtractor, StringExtractor)
    extract[DuplicateTypeParamClass[Int]](IntExtractor, IntExtractor)
    extract[MixedTypeParamClass[Int]](StringExtractor, IntExtractor)
    extract[List[String]](StringExtractor, StringExtractor)
    extract[Map[Int, String]](IntExtractor -> StringExtractor, IntExtractor -> StringExtractor)
  }

  it should "handle path-dependent types correctly" in {
    pending
    // TODO: This should compile, but doesn't due to a bug related to path dependent types:
    // extract[PathDependentOneTwo](
    //   one = extract[PathDependentOne](
    //     a = IntExtractor,
    //     b = StringExtractor
    //   ),
    //   two = extract[PathDependentTwo](
    //     a = StringExtractor,
    //     b = IntExtractor
    //   )
    // )
  }

  "nested extract[A]" should "have the correct extractHeadOption behaviour" in {
    nestedExtractor.extractHeadOption(tupleList) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("a", Circle))
    ))
  }

  it should "have the correct extractAll behaviour" in {
    nestedExtractor.extractAll(tupleList) should equal(List(
      AggregateOneTwo(One(1, "a"), Two("a", Circle)),
      AggregateOneTwo(One(3, "c"), Two("c", Tetrahedron)),
      AggregateOneTwo(One(-1, "e"), Two("e", Plane))
    ))
  }

  it should "support findCellExtractor syntax" in {
    nestedExtractor.findCellExtractor("one.a") should equal(Some(IntExtractor))
    nestedExtractor.findCellExtractor("two.b") should equal(Some(ShapeExtractor))
    nestedExtractor.findCellExtractor("two.c") should equal(None)
  }

  // TODO: Implement this test with illTyped:
  "extract[A]" should "fail if there are too few arguments" in {
    pending
    // extract[One](
    //   a = IntExtractor
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail if there are too many arguments" in {
    pending
    // extract[One](
    //   a = IntExtractor,
    //   b = IntExtractor,
    //   c = IntExtractor
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong types of arguments" in {
    pending
    // extract[One](
    //   a = StringExtractor,
    //   b = IntExtractor
    // )
  }

  // TODO: Implement this test with illTyped:
  it should "fail on the wrong argument names" in {
    pending
    // extract[One](
    //   b = IntExtractor,
    //   a = StringExtractor
    // )
  }

}
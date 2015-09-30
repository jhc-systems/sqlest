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
import shapeless.test.illTyped

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

  case class One(a: Int, b: String)
  case class Two(a: String, b: Shape)
  case class AggregateOneTwo(one: One, two: Two)
  case class Tiny(a: Shape)

  val shapeExtractor = new CellExtractor[Tuple3[Shape, Int, String], Shape] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._1)
  }

  val intExtractor = new CellExtractor[Tuple3[Shape, Int, String], Int] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._2)
  }

  val stringExtractor = new CellExtractor[Tuple3[Shape, Int, String], String] {
    def read(row: Tuple3[Shape, Int, String]) = Some(row._3)
  }

  val tupleRows = List(
    (Circle, 1, "a"),
    (Tetrahedron, 3, "c"),
    (Plane, -1, "e")
  )

  val simpleExtractor = extract[One](
    a = intExtractor,
    b = stringExtractor
  )

  "extract[A]" should "have the correct extractHeadOption behaviour" in {
    simpleExtractor.extractHeadOption(tupleRows) should equal(Some(
      One(1, "a")
    ))
  }

  it should "have the correct extractAll behaviour" in {
    simpleExtractor.extractAll(tupleRows) should equal(List(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "support findCellExtractor syntax" in {
    simpleExtractor.findCellExtractor("a") should equal(Some(intExtractor))
    simpleExtractor.findCellExtractor("b") should equal(Some(stringExtractor))
    simpleExtractor.findCellExtractor("c") should equal(None)
  }

  it should "work for case classes with one field" in {
    val extractor = extract[Tiny](a = shapeExtractor)

    extractor.extractHeadOption(tupleRows) should equal(Some(
      Tiny(Circle)
    ))

    extractor.extractAll(tupleRows) should equal(List(
      Tiny(Circle),
      Tiny(Tetrahedron),
      Tiny(Plane)
    ))
  }

  class Multiple(a: Int, b: Int)
  object Multiple {
    def apply(a: Int) = new Multiple(a, 37)
    def apply(a: Int, b: Int) = new Multiple(a, b)
    def apply(a: Int, b: Int, c: Int) = new Multiple(a, b + c)
  }
  it should "work for classes with multiple apply methods" in {
    extract[Multiple](a = intExtractor)
    extract[Multiple](a = intExtractor, b = intExtractor)
    extract[Multiple](a = intExtractor, b = intExtractor, c = intExtractor)
  }

  case class DefaultParams(a: Int, b: String = "sweet")
  it should "work for apply methods with default parameters" in {
    extract[DefaultParams](a = intExtractor, b = stringExtractor)
    extract[DefaultParams](a = intExtractor)
  }

  case class VarargsParams(a: Int, b: String*)
  it should "work for apply methods with varargs" in {
    extract[VarargsParams](intExtractor, stringExtractor, stringExtractor)
    extract[VarargsParams](intExtractor, stringExtractor)
    extract[VarargsParams](intExtractor)
  }

  case class TypeParamClass[A, B](a: A, b: B)
  case class ReversedTypeParamClass[A, B](b: B, a: A)
  case class DuplicateTypeParamClass[A](a1: A, a2: A)
  case class MixedTypeParamClass[A](s: String, a: A)
  it should "work for apply methods with type parameters" in {
    extract[TypeParamClass[String, Int]](stringExtractor, intExtractor)
    extract[TypeParamClass[String, Int]](stringExtractor, extractConstant(6))
    extract[ReversedTypeParamClass[String, Int]](intExtractor, stringExtractor)
    extract[DuplicateTypeParamClass[Int]](intExtractor, intExtractor)
    extract[MixedTypeParamClass[Int]](stringExtractor, intExtractor)
    extract[List[String]](stringExtractor, stringExtractor)
    extract[Map[Int, String]](intExtractor -> stringExtractor, intExtractor -> stringExtractor)
  }

  it should "handle path-dependent types correctly" in {
    pending
    // TODO: This should compile, but doesn't due to a bug related to path dependent types:
    // This is particularly hard because even if the basic version of this is fixed by using
    //   val enclosingType = c.prefix.actualType
    //   val typeOfA = weakTypeOf[A].asSeenFrom(enclosingType, enclosingType.typeSymbol.asClass)
    //   (arg: $tupleType) => ${companion.name.toTermName}.$applyMethod(..$tupleAccessors)
    // the apply method for the nested version refers to the path dependent inner types instead
    // of the resolved types and `asSeenFrom` doesn't work on those types

    // extract[PathDependentOne](
    //   a = intExtractor,
    //   b = stringExtractor
    // )

    // extract[PathDependentOneTwo](
    //   one = extract[PathDependentOne](
    //     a = intExtractor,
    //     b = stringExtractor
    //   ),
    //   two = extract[PathDependentTwo](
    //     a = stringExtractor,
    //     b = intExtractor
    //   )
    // )
  }

  val nestedExtractor = extract[AggregateOneTwo](
    one = extract[One](
      a = intExtractor,
      b = stringExtractor
    ),
    two = extract[Two](
      a = stringExtractor,
      b = shapeExtractor
    )
  )

  "nested extract[A]" should "have the correct extractHeadOption behaviour" in {
    nestedExtractor.extractHeadOption(tupleRows) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("a", Circle))
    ))
  }

  it should "have the correct extractAll behaviour" in {
    nestedExtractor.extractAll(tupleRows) should equal(List(
      AggregateOneTwo(One(1, "a"), Two("a", Circle)),
      AggregateOneTwo(One(3, "c"), Two("c", Tetrahedron)),
      AggregateOneTwo(One(-1, "e"), Two("e", Plane))
    ))
  }

  it should "support findCellExtractor syntax" in {
    nestedExtractor.findCellExtractor("one.a") should equal(Some(intExtractor))
    nestedExtractor.findCellExtractor("two.b") should equal(Some(shapeExtractor))
    nestedExtractor.findCellExtractor("two.c") should equal(None)
  }

  "extract[A]" should "fail if there are too few arguments" in {
    illTyped("""
      extract[One](
        a = intExtractor
      )
              """)
  }

  it should "fail if there are too many arguments" in {
    illTyped("""
      extract[One](
        a = intExtractor,
        b = intExtractor,
        c = intExtractor
      )
              """)
  }

  it should "fail on the wrong types of arguments" in {
    illTyped("""
      extract[One](
        a = stringExtractor,
        b = intExtractor
      )
              """)
  }

  it should "fail on the wrong argument names" in {
    illTyped("""
      extract[One](
        b = intExtractor,
        a = stringExtractor
      )
              """)
  }

  it should "complete successfully on a second call to extract" in {
    import scala.language.reflectiveCalls

    val twoExtractor = extract[Two](
      a = stringExtractor,
      b = shapeExtractor
    )

    val badNestedExtractor = extract[AggregateOneTwo](
      one = extract[One](
        a = intExtractor,
        b = stringExtractor
      ),
      two = extract(twoExtractor)
    )

    badNestedExtractor.extractHeadOption(tupleRows) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("a", Circle))
    ))

    /* TODO: make the following test pass
    illTyped("""
      extract[AggregateOneTwo](
        one = extract[One](
          a = intExtractor,
          b = stringExtractor
        ),
        two = extract(twoExtractor)
      )
      """) */
  }

  sealed trait Extracted
  case class LeftExtracted(i: Int, s: String) extends Extracted
  case class RightExtracted(i: Int, s: String) extends Extracted
  it should "work with ChoiceExtractor to extract case classes based upon a predicate" in {
    val choiceExtractor = extractTuple(shapeExtractor, intExtractor, stringExtractor).choose(_._1 == Tetrahedron)(
      extract[LeftExtracted](
        intExtractor,
        stringExtractor
      ),
      extract[RightExtracted](
        intExtractor,
        stringExtractor
      )
    )

    choiceExtractor.extractHeadOption(tupleRows) should equal(Some(RightExtracted(1, "a")))

    choiceExtractor.extractAll(tupleRows) should equal(List(
      RightExtracted(1, "a"),
      LeftExtracted(3, "c"),
      RightExtracted(-1, "e")
    ))
  }
}
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

import org.joda.time.{ DateTime, LocalDate }
import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.ast.Setter

class FirstTable(alias: Option[String]) extends Table("first", alias) {
  val col1 = column[Int]("col1")
  val col2 = column[String]("col2")
  val col3 = column[Option[String]]("col3")
  val col4 = column[Option[Int]]("col4")
  val col5 = column[Int]("col5")
}
object FirstTable extends FirstTable(None)

case class One(a: Int, b: String)
case class Two(a: Option[String], b: Option[Int])
case class AggregateOneTwo(one: Option[One], two: Two)
case class VarargsParams(a: Int, b: Int*)
case class TypeParamClass[A, B](a: A, b: B)

class Multiple(val a: Int, val b: Int)
object Multiple {
  def apply(a: Int) = new Multiple(a, 37)
  def apply(a: Int, b: Int) = new Multiple(a, b)
  def unapply(multiple: Multiple) = Some((multiple.a, multiple.b))
}

case class DefaultParams(a: Int, b: String = "sweet")

class ColumnExtractorSettersSpec extends FlatSpec with Matchers {

  val oneExtractor = extract[One](
    a = FirstTable.col1,
    b = FirstTable.col2
  )

  val twoExtractor = extract[Two](
    a = FirstTable.col3,
    b = FirstTable.col4
  )

  val aggregateOneTwoExtractor = extract[AggregateOneTwo](
    one = oneExtractor.asOption,
    two = twoExtractor
  )

  "settersFor" should "return setters for a case class and its extractor" in {
    oneExtractor.settersFor(One(1, "one")) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "one")
    ))
  }

  it should "return setters for nested case class extractors" in {
    aggregateOneTwoExtractor.settersFor(AggregateOneTwo(Some(One(1, "one")), Two(Some("two"), None))) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "one"),
      Setter(FirstTable.col3, Some("two")),
      Setter(FirstTable.col4, Option.empty[Int])
    ))
  }

  it should "return setters for tuple extractors" in {
    extractTuple(FirstTable.col1, FirstTable.col3).settersFor((1, None)) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col3, Option.empty[String])
    ))
  }

  it should "return setters for extractors using asNonOption" in {
    val oneNonOptionExtractor = extract[One](
      a = FirstTable.col4.asNonOption,
      b = FirstTable.col3.asNonOption
    )

    oneNonOptionExtractor.settersFor(One(1, "one")) should be(List(
      Setter(FirstTable.col4, 1),
      Setter(FirstTable.col3, "one")
    ))

    val aggregateOneNonOptionTwoExtractor = extract[AggregateOneTwo](
      one = oneNonOptionExtractor.asOption,
      two = twoExtractor
    )

    aggregateOneNonOptionTwoExtractor.settersFor(AggregateOneTwo(None, Two(Some("two"), None))) should be(List(
      Setter(FirstTable.col4, Option.empty[Int]),
      Setter(FirstTable.col3, Option.empty[String]),
      Setter(FirstTable.col3, Some("two")),
      Setter(FirstTable.col4, Option.empty[Int])
    ))

    aggregateOneNonOptionTwoExtractor.settersFor(AggregateOneTwo(Some(One(1, "one")), Two(Some("two"), None))) should be(List(
      Setter(FirstTable.col4, 1),
      Setter(FirstTable.col3, "one"),
      Setter(FirstTable.col3, Some("two")),
      Setter(FirstTable.col4, Option.empty[Int])
    ))
  }

  it should "return setters for class extractors with multiple apply methods and an unapply method" in {
    extract[Multiple](FirstTable.col1, FirstTable.col5).settersFor(Multiple(1)) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col5, 37)
    ))
  }

  it should "return setters for case class extractors with default parameters" in {
    extract[DefaultParams](FirstTable.col1).settersFor(DefaultParams(1)) should be(List(
      Setter(FirstTable.col1, 1)
    ))

    extract[DefaultParams](FirstTable.col1, FirstTable.col2).settersFor(DefaultParams(1)) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "sweet")
    ))
  }

  it should "return setters for option extractors only for Some" in {
    aggregateOneTwoExtractor.settersFor(AggregateOneTwo(Some(One(1, "one")), Two(Some("two"), None))) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "one"),
      Setter(FirstTable.col3, Some("two")),
      Setter(FirstTable.col4, Option.empty[Int])
    ))

    aggregateOneTwoExtractor.settersFor(AggregateOneTwo(None, Two(Some("two"), None))) should be(List(
      Setter(FirstTable.col3, Some("two")),
      Setter(FirstTable.col4, Option.empty[Int])
    ))
  }

  it should "return setters for case class extractors with varargs parameters" in {
    extract[VarargsParams](FirstTable.col1, FirstTable.col5).settersFor(VarargsParams(1, 3, 5)) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col5, 3)
    ))

    extract[List[Int]](FirstTable.col1, FirstTable.col5).settersFor(List(1, 3)) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col5, 3)
    ))
  }

  it should "return setters for case class with type parameters" in {
    extract[TypeParamClass[Int, String]](FirstTable.col1, FirstTable.col2).settersFor(TypeParamClass(1, "two")) should be(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "two")
    ))
  }

  it should "fail throw an exception when used with other extractors" in {
    intercept[Exception] {
      oneExtractor.groupBy(FirstTable.col1).settersFor(List(One(1, "one")))
    }

    intercept[Exception] {
      extractTuple(FirstTable.col1, FirstTable.col2.asList).settersFor((1, Nil))
    }

    intercept[Exception] {
      extractTuple(FirstTable.col1, FirstTable.col2).map(_.toString ++ _).settersFor("bad")
    }
  }

  it should "work for lists of values" in {
    oneExtractor.settersFor(List(One(1, "one"), One(2, "two"))) should be(List(List(
      Setter(FirstTable.col1, 1),
      Setter(FirstTable.col2, "one")
    ), List(
      Setter(FirstTable.col1, 2),
      Setter(FirstTable.col2, "two")
    )))
  }
}

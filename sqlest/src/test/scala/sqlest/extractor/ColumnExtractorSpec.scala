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

import org.joda.time.DateTime
import org.scalatest._
import org.scalatest.matchers._
import sqlest._

class ColumnExtractorSpec extends FlatSpec with Matchers {
  import TestData._

  case class Inner(b: Int, c: List[Int])
  case class Outer(a: Int, b: List[Inner])
  case class Flattened(a: Int, b: List[Int], c: List[Int])
  case class AggregateOnePointFive(one: One, str: String)
  case class DefaultParams(a: Int, b: String = "sweet")
  case class VarargsParams(a: Int, b: String*)
  case class TypeParamClass[A, B](a: A, b: B)
  case class ReversedTypeParamClass[A, B](b: B, a: A)
  case class DuplicateTypeParamClass[A](a1: A, a2: A)
  case class MixedTypeParamClass[A](s: String, a: A)

  "columns" should "extract the column type" in {
    val date = new java.sql.Date(new java.util.Date().getTime)
    def results = TestResultSet(TableFive.columns)(
      Seq(1, 10000000000L, 3.14, new java.math.BigDecimal("2.818"), true, "Hello mars", date, Array[Byte](1, 127))
    )

    TableFive.intCol.extractHeadOption(results) should be(Some(1))
    TableFive.longCol.extractHeadOption(results) should be(Some(10000000000L))
    TableFive.doubleCol.extractHeadOption(results) should be(Some(3.14))
    TableFive.bigDecimalCol.extractHeadOption(results) should be(Some(BigDecimal(2.818)))
    TableFive.booleanColumn.extractHeadOption(results) should be(Some(true))
    TableFive.stringColumn.extractHeadOption(results) should be(Some("Hello mars"))
    TableFive.dateTimeCol.extractHeadOption(results) should be(Some(new DateTime(date)))
    TableFive.byteArrayCol.extractHeadOption(results).map(_.toList) should be(Some(Array[Byte](1, 127).toList))
  }

  "mapped column extractor" should "extract mapped value" in {
    val extractor = extractTuple(TableSix.trimmedString, TableSix.zeroIsNoneWrappedInt, TableSix.zeroIsNoneDateTime)

    def testResultSet = TestResultSet(TableSix.columns)(
      Seq("test", 5, 20150101),
      Seq(" test ", 0, 21000101),
      Seq("   ", 0, 0)
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      (Some(WrappedString("test")), Some(WrappedInt(5)), Some(new DateTime(2015, 1, 1, 0, 0)))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      (Some(WrappedString("test")), Some(WrappedInt(5)), Some(new DateTime(2015, 1, 1, 0, 0))),
      (Some(WrappedString("test")), None, Some(new DateTime(2100, 1, 1, 0, 0))),
      (None, None, None)
    ))
  }

  "single case class extractor" should "extract appropriate data structures" in {
    val extractor = extract[One](
      a = TableOne.col1,
      b = TableOne.col2
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      One(1, "a")
    ))

    extractor.extractAll(testResultSet) should equal(List(
      One(1, "a"),
      One(3, "c"),
      One(-1, "e")
    ))
  }

  it should "work for default parameters" in {
    val extractor = extract[DefaultParams](
      a = TableOne.col1
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      DefaultParams(1, "sweet")
    ))

    extractor.extractAll(testResultSet) should equal(List(
      DefaultParams(1, "sweet"),
      DefaultParams(3, "sweet"),
      DefaultParams(-1, "sweet")
    ))
  }

  it should "work for apply methods with varargs" in {
    val extractor1 = extract[VarargsParams](TableOne.col1)
    extractor1.extractHeadOption(testResultSet) should equal(Some(
      VarargsParams(1)
    ))

    extractor1.extractAll(testResultSet) should equal(List(
      VarargsParams(1),
      VarargsParams(3),
      VarargsParams(-1)
    ))

    val extractor3 = extract[VarargsParams](TableOne.col1, TableOne.col2, TableTwo.col2, TableOne.col2)
    extractor3.extractHeadOption(testResultSet) should equal(Some(
      VarargsParams(1, "a", "b", "a")
    ))

    extractor3.extractAll(testResultSet) should equal(List(
      VarargsParams(1, "a", "b", "a"),
      VarargsParams(3, "c", "d", "c"),
      VarargsParams(-1, "e", "f", "e")
    ))
  }

  it should "work for apply methods with type parameters" in {
    val extractor1 = extract[TypeParamClass[String, Int]](TableOne.col2, TableOne.col1)
    extractor1.extractHeadOption(testResultSet) should equal(Some(
      TypeParamClass("a", 1)
    ))

    extractor1.extractAll(testResultSet) should equal(List(
      TypeParamClass("a", 1),
      TypeParamClass("c", 3),
      TypeParamClass("e", -1)
    ))

    val extractor2 = extract[List[String]](TableOne.col2, TableTwo.col2)
    extractor2.extractHeadOption(testResultSet) should equal(Some(
      List("a", "b")
    ))

    extractor2.extractAll(testResultSet) should equal(List(
      List("a", "b"),
      List("c", "d"),
      List("e", "f")
    ))

    val extractor3 = extract[ReversedTypeParamClass[String, Int]](TableOne.col1, TableOne.col2)
    extractor3.extractHeadOption(testResultSet) should equal(Some(
      ReversedTypeParamClass(1, "a")
    ))

    extractor3.extractAll(testResultSet) should equal(List(
      ReversedTypeParamClass(1, "a"),
      ReversedTypeParamClass(3, "c"),
      ReversedTypeParamClass(-1, "e")
    ))

    val extractor4 = extract[DuplicateTypeParamClass[Int]](TableOne.col1, TableTwo.col3)
    extractor4.extractHeadOption(testResultSet) should equal(Some(
      DuplicateTypeParamClass(1, 2)
    ))

    extractor4.extractAll(testResultSet) should equal(List(
      DuplicateTypeParamClass(1, 2),
      DuplicateTypeParamClass(3, 4),
      DuplicateTypeParamClass(-1, 6)
    ))

    val extractor5 = extract[MixedTypeParamClass[Int]](TableTwo.col2, TableOne.col1)
    extractor5.extractHeadOption(testResultSet) should equal(Some(
      MixedTypeParamClass("b", 1)
    ))

    extractor5.extractAll(testResultSet) should equal(List(
      MixedTypeParamClass("b", 1),
      MixedTypeParamClass("d", 3),
      MixedTypeParamClass("f", -1)
    ))
  }

  "tuple extractor" should "extract appropriate data structures" in {
    val extractor = extractTuple(TableOne.col1, TableOne.col2)

    extractor.extractHeadOption(testResultSet) should equal(Some(
      (1, "a")
    ))

    extractor.extractAll(testResultSet) should equal(List(
      (1, "a"),
      (3, "c"),
      (-1, "e")
    ))
  }

  it should "work with inherited apply methods" in {
    val extractor = extract[Map[Int, String]](TableOne.col1 -> TableOne.col2, TableTwo.col3 -> TableTwo.col2)
    extractor.extractHeadOption(testResultSet) should equal(Some(
      Map(1 -> "a", 2 -> "b")
    ))

    extractor.extractAll(testResultSet) should equal(List(
      Map(1 -> "a", 2 -> "b"),
      Map(3 -> "c", 4 -> "d"),
      Map(-1 -> "e", 6 -> "f")
    ))
  }

  "aggregate case class extractor" should "extract appropriate data structures" in {
    val extractor = extract[AggregateOneTwo](
      one = extract[One](
        a = TableOne.col1,
        b = TableOne.col2
      ),
      two = extract[Two](
        a = TableTwo.col2,
        b = TableTwo.col3
      )
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("b", 2))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      AggregateOneTwo(One(1, "a"), Two("b", 2)),
      AggregateOneTwo(One(3, "c"), Two("d", 4)),
      AggregateOneTwo(One(-1, "e"), Two("f", 6))
    ))
  }

  "list extractor" should "extract appropriate data structures" in {
    val extractor = extract[AggregateOneTwo](
      one = extract[One](
        a = TableOne.col1,
        b = TableOne.col2
      ),
      two = extract[Two](
        a = TableTwo.col2,
        b = TableTwo.col3
      )
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      AggregateOneTwo(One(1, "a"), Two("b", 2))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      AggregateOneTwo(One(1, "a"), Two("b", 2)),
      AggregateOneTwo(One(3, "c"), Two("d", 4)),
      AggregateOneTwo(One(-1, "e"), Two("f", 6))
    ))
  }

  "non option extractors" should "throw exceptions when extracting nulls" in {
    def results = TestResultSet(TableFive.columns)(
      Seq(null, null, null, null, null, null, null, null)
    )

    intercept[NullPointerException] {
      TableFive.intCol.extractHeadOption(results)
    }

    intercept[NullPointerException] {
      TableFive.dateTimeCol.extractHeadOption(results)
    }

    intercept[NullPointerException] {
      TableFive.bigDecimalCol.extractHeadOption(results)
    }

    intercept[NullPointerException] {
      TableFive.byteArrayCol.extractHeadOption(results)
    }

    intercept[NullPointerException] {
      extractTuple(
        TableFive.intCol,
        TableFive.stringColumn
      ).extractHeadOption(results)
    }

    intercept[NullPointerException] {
      extractTuple(
        TableFive.intCol.asOption,
        TableFive.stringColumn
      ).extractHeadOption(results)
    }

    intercept[NullPointerException] {
      extractTuple(
        TableFive.intCol,
        TableFive.stringColumn.asOption
      ).extractHeadOption(results)
    }

    intercept[NullPointerException] {
      extract[One](
        a = TableFive.intCol,
        b = TableFive.stringColumn
      ).extractHeadOption(results)
    }
  }

  "list extractors" should "allow only full or empty lists" in {
    def emptySubListresults = TestResultSet(TableOne.columns)(
      Seq(1, null),
      Seq(1, null)
    )

    def partlyFullSubListResults = TestResultSet(TableOne.columns)(
      Seq(1, "b"),
      Seq(1, null)
    )

    def fullSubListresults = TestResultSet(TableOne.columns)(
      Seq(1, "b"),
      Seq(1, "b")
    )

    val extractor = extractTuple(
      TableOne.col1,
      TableOne.col2.asList
    ).groupBy(TableOne.col1)

    extractor.extractHeadOption(emptySubListresults)

    intercept[NullPointerException] {
      extractor.extractHeadOption(partlyFullSubListResults)
    }

    extractor.extractHeadOption(fullSubListresults)
  }

  "option column types" should "handle nulls" in {
    val extractor = extract[AggregateOneTwoThree](
      one = extract[One](
        a = TableOne.col1,
        b = TableOne.col2
      ),
      two = extract[Two](
        a = TableTwo.col2,
        b = TableTwo.col3
      ),
      three = extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      )
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      AggregateOneTwoThree(One(1, "a"), Two("b", 2), Three(None, Some("x")))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      AggregateOneTwoThree(One(1, "a"), Two("b", 2), Three(None, Some("x"))),
      AggregateOneTwoThree(One(3, "c"), Two("d", 4), Three(Some(9), None)),
      AggregateOneTwoThree(One(-1, "e"), Two("f", 6), Three(None, None))
    ))
  }

  "option extractor" should "handle nulls" in {
    def results = TestResultSet(TableOne.columns)(
      Seq(1, "q   "),
      Seq(3, null),
      Seq(null, "e   ")
    )

    val extractor1 = TableOne.col1.asOption

    extractor1.extractHeadOption(results) should equal(Some(
      Some(1)
    ))

    extractor1.extractAll(results) should equal(List(
      Some(1),
      Some(3),
      None
    ))

    val extractor2 = TableOne.col2.?

    extractor2.extractHeadOption(results) should equal(Some(
      Some("q")
    ))

    extractor2.extractAll(results) should equal(List(
      Some("q"),
      None,
      Some("e")
    ))
  }

  "option extractor" should "handle nulls for compound tuples" in {
    val extractor = extractTuple(TableThree.col3, TableThree.col4).asOption

    extractor.extractHeadOption(testResultSet) should equal(Some(
      Some((None, Some("x")))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      Some((None, Some("x"))),
      Some((Some(9), None)),
      Some((None, None))
    ))
  }

  "option extractor" should "handle nulls for compound values" in {
    val extractor = extract[Three](
      a = TableThree.col3,
      b = TableThree.col4
    ).asOption

    extractor.extractHeadOption(testResultSet) should equal(Some(
      Some(Three(None, Some("x")))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      Some(Three(None, Some("x"))),
      Some(Three(Some(9), None)),
      Some(Three(None, None))
    ))
  }

  "option extractor" should "handle nulls for nested compound values" in {
    val extractor = extract[AggregateOneTwoOptionThree](
      one = extract[One](
        a = TableOne.col1,
        b = TableOne.col2
      ),
      two = extract[Two](
        a = TableTwo.col2,
        b = TableTwo.col3
      ),
      three = extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      ).asOption
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      AggregateOneTwoOptionThree(One(1, "a"), Two("b", 2), Some(Three(None, Some("x"))))
    ))

    extractor.extractAll(testResultSet) should equal(List(
      AggregateOneTwoOptionThree(One(1, "a"), Two("b", 2), Some(Three(None, Some("x")))),
      AggregateOneTwoOptionThree(One(3, "c"), Two("d", 4), Some(Three(Some(9), None))),
      AggregateOneTwoOptionThree(One(-1, "e"), Two("f", 6), Some(Three(None, None)))
    ))
  }

  "columns, constants and extractors" should "be usable together as arguments to `extractor`" in {
    val extractor = extract[AggregateOnePointFive](
      one = extract[One](
        a = TableOne.col1,
        b = extractConstant("const")
      ),
      str = TableTwo.col2
    )

    extractor.extractHeadOption(testResultSet) should equal(Some(
      AggregateOnePointFive(One(1, "const"), "b")
    ))

    extractor.extractAll(testResultSet) should equal(List(
      AggregateOnePointFive(One(1, "const"), "b"),
      AggregateOnePointFive(One(3, "const"), "d"),
      AggregateOnePointFive(One(-1, "const"), "f")
    ))
  }

  "mapped columns" should "extract the correct type" in {
    def results = TestResultSet(TableFour.columns)(
      Seq("Y"),
      Seq("N")
    )

    val extractor = TableFour.mapped

    extractor.extractHeadOption(results) should equal(Some(true))

    extractor.extractAll(results) should equal(List(true, false))
  }

  "nested list extractor" should "stop when the left value changes" in {
    def results = TestResultSet(TableOne.columns ++ TableTwo.columns)(
      Seq(1, "a", "a", 1),
      Seq(1, "a", "b", 2),
      Seq(2, "b", "c", 3)
    )

    val extractor = extractTuple(
      extract[One](
        a = TableOne.col1,
        b = TableOne.col2
      ),
      extract[Two](
        a = TableTwo.col2,
        b = TableTwo.col3
      ).asList
    ).groupBy(extractTuple(
        TableOne.col1,
        TableOne.col2
      ))

    extractor.extractHeadOption(results) should equal(Some(
      (One(1, "a"), List(Two("a", 1), Two("b", 2)))
    ))

    extractor.extractAll(results) should equal(List(
      (One(1, "a"), List(Two("a", 1), Two("b", 2))),
      (One(2, "b"), List(Two("c", 3)))
    ))
  }

  "nested list extractor" should "handle columns with nullable fields" in {
    object TableThreeB extends TableThree(Some("b"))

    def results = TestResultSet(TableThree.columns ++ TableThreeB.columns)(
      Seq(1, null, 4, null),
      Seq(1, null, null, null),
      Seq(null, "b", 6, null)
    )

    val extractor = extractTuple(
      extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      ),
      extract[Three](
        a = TableThreeB.col3,
        b = TableThreeB.col4
      ).asList
    ).groupBy(TableThree.col3)

    extractor.extractHeadOption(results) should equal(Some(
      (Three(Some(1), None), List(Three(Some(4), None), Three(None, None)))
    ))

    extractor.extractAll(results) should equal(List(
      (Three(Some(1), None), List(Three(Some(4), None), Three(None, None))),
      (Three(None, Some("b")), List(Three(Some(6), None)))
    ))
  }

  "nested list extractor" should "be composable with option extractor" in {
    object TableThreeB extends TableThree(Some("b"))

    def results = TestResultSet(TableThree.columns ++ TableThreeB.columns)(
      Seq(1, null, 4, null),
      Seq(1, null, null, null),
      Seq(null, "b", 6, null)
    )

    val extractor = extractTuple(
      extract[Three](
        a = TableThree.col3,
        b = TableThree.col4
      ),
      extract[Three](
        a = TableThreeB.col3,
        b = TableThreeB.col4
      ).asOption.asList
    ).groupBy(TableThree.col3)

    extractor.extractHeadOption(results) should equal(Some(
      (Three(Some(1), None), List(Some(Three(Some(4), None)), Some(Three(None, None))))
    ))

    extractor.extractAll(results) should equal(List(
      (Three(Some(1), None), List(Some(Three(Some(4), None)), Some(Three(None, None)))),
      (Three(None, Some("b")), List(Some(Three(Some(6), None))))
    ))
  }

  "list extractor" should "work as peers within a tuple extractor" in {
    class TestTable(alias: Option[String]) extends Table("n", alias) {
      val col1 = column[Int]("col1")
      val col2 = column[Int]("col2")
      val col3 = column[Int]("col3")
    }

    object TestTable extends TestTable(None)

    import TestTable._

    def results = TestResultSet(List(col1, col2, col3))(
      Seq(1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5),
      Seq(2, 3, 6),
      Seq(2, 4, 7),
      Seq(2, 4, 8)
    )

    val extractor = extractTuple(
      col1,
      col2.asList,
      col3.asList
    ).groupBy(col1)

    extractor.extractHeadOption(results) should equal(Some(
      (
        1,
        List(1, 1, 2, 2),
        List(1, 2, 3, 4)
      )
    ))

    extractor.extractAll(results) should equal(List(
      (
        1,
        List(1, 1, 2, 2),
        List(1, 2, 3, 4)
      ),
      (
        2,
        List(3, 3, 4, 4),
        List(5, 6, 7, 8)
      )
    ))
  }

  "list extractor" should "work as peers within a mapped extractor" in {
    class TestTable(alias: Option[String]) extends Table("n", alias) {
      val col1 = column[Int]("col1")
      val col2 = column[Int]("col2")
      val col3 = column[Int]("col3")
    }

    object TestTable extends TestTable(None)

    import TestTable._

    def results = TestResultSet(List(col1, col2, col3))(
      Seq(1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5),
      Seq(2, 3, 6),
      Seq(2, 4, 7),
      Seq(2, 4, 8)
    )

    val extractor = extract[Flattened](
      a = col1,
      b = col2.asList,
      c = col3.asList
    ).groupBy(col1)

    extractor.extractHeadOption(results) should equal(Some(
      Flattened(
        1,
        List(1, 1, 2, 2),
        List(1, 2, 3, 4)
      )
    ))

    extractor.extractAll(results) should equal(List(
      Flattened(
        1,
        List(1, 1, 2, 2),
        List(1, 2, 3, 4)
      ),
      Flattened(
        2,
        List(3, 3, 4, 4),
        List(5, 6, 7, 8)
      )
    ))
  }

  "list extractor" should "work in a nested left join without groupBy" in {
    class TestTable(alias: Option[String]) extends Table("n", alias) {
      val col1 = column[Int]("col1")
      val col2 = column[Int]("col2")
      val col3 = column[Int]("col3")
    }

    object TestTable extends TestTable(None)

    import TestTable._

    def results = TestResultSet(List(col1, col2, col3))(
      Seq(1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5),
      Seq(2, 3, 6),
      Seq(2, 4, 7),
      Seq(2, 4, 8)
    )

    val extractor = extract[Outer](
      a = col1,
      b = extract[Inner](
        b = col2,
        c = col3.asList
      ).asList
    )

    extractor.extractHeadOption(results) should equal(Some(
      Outer(1, List(Inner(1, List(1))))
    ))

    extractor.extractAll(results) should equal(List(
      Outer(1, List(Inner(1, List(1)))),
      Outer(1, List(Inner(1, List(2)))),
      Outer(1, List(Inner(2, List(3)))),
      Outer(1, List(Inner(2, List(4)))),
      Outer(2, List(Inner(3, List(5)))),
      Outer(2, List(Inner(3, List(6)))),
      Outer(2, List(Inner(4, List(7)))),
      Outer(2, List(Inner(4, List(8))))
    ))
  }

  "list extractor" should "work in a nested left join with outer groupBy" in {
    class TestTable(alias: Option[String]) extends Table("n", alias) {
      val col1 = column[Int]("col1")
      val col2 = column[Int]("col2")
      val col3 = column[Int]("col3")
    }

    object TestTable extends TestTable(None)

    import TestTable._

    def results = TestResultSet(List(col1, col2, col3))(
      Seq(1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5),
      Seq(2, 3, 6),
      Seq(2, 4, 7),
      Seq(2, 4, 8)
    )

    val extractor = extract[Outer](
      a = col1,
      b = extract[Inner](
        b = col2,
        c = col3.asList
      ).asList
    ).groupBy(col1)

    extractor.extractHeadOption(results) should equal(Some(Outer(1, List(
      Inner(1, List(1)),
      Inner(1, List(2)),
      Inner(2, List(3)),
      Inner(2, List(4))))))

    extractor.extractAll(results) should equal(List(
      Outer(
        1,
        List(
          Inner(1, List(1)),
          Inner(1, List(2)),
          Inner(2, List(3)),
          Inner(2, List(4)))),
      Outer(
        2,
        List(
          Inner(3, List(5)),
          Inner(3, List(6)),
          Inner(4, List(7)),
          Inner(4, List(8))))))
  }

  "list extractor" should "return empty list for null rows" in {
    class TestTable(alias: Option[String]) extends Table("n", alias) {
      val col1 = column[Int]("col1")
      val col2 = column[Int]("col2")
      val col3 = column[Int]("col3")
    }

    object TestTable extends TestTable(None)

    import TestTable._

    def results = TestResultSet(List(col1, col2, col3))(
      Seq(1, 1, null),
      Seq(1, 2, null),
      Seq(2, 3, null),
      Seq(2, 4, null)
    )

    val extractor = extractTuple(
      col1,
      col2.asList,
      col3.asList
    ).groupBy(col1)

    extractor.extractHeadOption(results) should equal(Some(
      (1, List(1, 2), List())
    ))

    extractor.extractAll(results) should equal(List(
      (1, List(1, 2), List()),
      (2, List(3, 4), List())
    ))
  }

  "scalar function extractor" should "extract value" in {
    val aliasedScalarFunctionColumn = sqlest.ast.AliasColumn[Int](null, "scalarFunction")

    def results = TestResultSet(Seq(aliasedScalarFunctionColumn))(
      Seq(10)
    )

    val extractor = extractColumnByName[Int]("scalarFunction")

    extractor.extractHeadOption(results) should equal(Some(10))

    extractor.extractAll(results) should equal(List(10))
  }

  "scalar function extractor" should "compose with other extractors" in {
    val aliasedScalarFunctionColumn = sqlest.ast.AliasColumn[Int](null, "scalarFunction")

    def results = TestResultSet(TableThree.columns ++ Seq(aliasedScalarFunctionColumn))(
      Seq(1, "b", 10)
    )

    val extractor = extractTuple(
      TableThree.col3,
      TableThree.col4,
      extractColumnByName[Int]("scalarFunction").asOption
    )

    extractor.extractHeadOption(results) should equal(Some(
      (Some(1), Some("b"), Some(10))
    ))

    extractor.extractAll(results) should equal(List(
      (Some(1), Some("b"), Some(10))
    ))
  }

}

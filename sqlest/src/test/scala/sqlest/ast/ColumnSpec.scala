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

package sqlest.ast

import org.scalatest._
import org.scalatest.matchers._
import sqlest._

class ColumnSpec extends FlatSpec with Matchers {

  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size
  object Size {
    implicit val sizeColumnType = EnumerationColumnType[Size, String](Small -> "S", Medium -> "M", Large -> "L")
  }

  case class WrappedInt(int: Int)
  object WrappedInt {
    // Test that ColumnType[WrappedInt] can be materialized when 2 apply methods exist
    def apply(int1: Int, int2: Int) = new WrappedInt(int1 + int2)
  }

  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[String]("col2")
    val col3 = column[Int]("col3")(MappedColumnType[Int, String](_.toInt, _.toString))
    val col4 = column[Double]("col1")
    val col5 = column[Option[String]]("col5")(OptionColumnType(""))
    val col6 = column[Size]("col6")
    val col7 = column[WrappedInt]("col7")

    def * = (col1, col2)
  }

  object TableOne extends TableOne(None)

  "option column equivalences" should "allow binary operators involving base types and their option types" in {
    (literalColumn(1) =!= 2) should equal(InfixFunctionColumn[Boolean]("<>", 1, 2))
    (literalColumn(Some(1)) =!= 2) should equal(InfixFunctionColumn[Boolean]("<>", Some(1), 2))
    (literalColumn(1) =!= Option.empty[Int]) should equal(InfixFunctionColumn[Boolean]("<>", 1, Option.empty[Int]))
    (literalColumn(Some(1)) =!= Option.empty[Int]) should equal(InfixFunctionColumn[Boolean]("<>", Some(1), Option.empty[Int]))
  }

  "column equivalences" should "allow operations between different numeric types" in {
    TableOne.col1 >= TableOne.col4
    TableOne.col4 === 1
    TableOne.col4 === 1.constant
    TableOne.col1 + TableOne.col4
    TableOne.col3 / 1.23
    TableOne.col3 / 1.23.constant
    TableOne.col1.between(TableOne.col4, 5)
    TableOne.col1.between(TableOne.col4, 5.constant)
  }

  "option column equivalences" should "allow binary operators involving mapped and unmapped types" in {
    val expr1 = (TableOne.col1 === 1)
    expr1 should equal(InfixFunctionColumn[Boolean]("=", TableOne.col1, 1))
    expr1.parameter2.columnType should equal(TableOne.col1.columnType)

    val expr2 = (TableOne.col3 === 1)
    expr2 should equal(InfixFunctionColumn[Boolean]("=", TableOne.col3, "1"))
    expr2.parameter2.columnType should equal(TableOne.col2.columnType)

    val expr3 = (TableOne.col5 === "Hi!")
    expr3 should equal(InfixFunctionColumn[Boolean]("=", TableOne.col5, "Hi!"))
    expr3.parameter2.columnType should equal(TableOne.col2.columnType)
  }

  "mapped column types" should "allow comparison operations" in {
    (TableOne.col6 === Small.constant[Size]) should equal(InfixFunctionColumn[Boolean]("=", TableOne.col6, Small.constant[Size]))
    (TableOne.col6 =!= Small.constant[Size]) should equal(InfixFunctionColumn[Boolean]("<>", TableOne.col6, Small.constant[Size]))
    (TableOne.col6 < Small.constant[Size]) should equal(InfixFunctionColumn[Boolean]("<", TableOne.col6, Small.constant[Size]))
    (TableOne.col6 <= Small.constant[Size]) should equal(InfixFunctionColumn[Boolean]("<=", TableOne.col6, Small.constant[Size]))
    (TableOne.col6 > Small.constant[Size]) should equal(InfixFunctionColumn[Boolean](">", TableOne.col6, Small.constant[Size]))
    (TableOne.col6 >= Small.constant[Size]) should equal(InfixFunctionColumn[Boolean](">=", TableOne.col6, Small.constant[Size]))
    (TableOne.col6.in(Small.constant[Size], Medium.constant[Size])) should equal(InfixFunctionColumn[Boolean]("in", TableOne.col6, ScalarFunctionColumn[Size]("", Seq(Small.constant[Size], Medium.constant[Size]))))
    (TableOne.col6 in List[Size](Small, Medium)) should equal(InfixFunctionColumn[Boolean]("in", TableOne.col6, ScalarFunctionColumn[Size]("", Seq(Small.constant[Size], Medium.constant[Size]))))

    (TableOne.col7 === WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean]("=", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7 =!= WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean]("<>", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7 < WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean]("<", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7 <= WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean]("<=", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7 > WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean](">", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7 >= WrappedInt(3).constant) should equal(InfixFunctionColumn[Boolean](">=", TableOne.col7, WrappedInt(3).constant))
    (TableOne.col7.in(WrappedInt(0).constant, WrappedInt(1).constant)) should equal(InfixFunctionColumn[Boolean]("in", TableOne.col7, ScalarFunctionColumn[WrappedInt]("", Seq(WrappedInt(0).constant, WrappedInt(1).constant))))
    (TableOne.col7 in List(WrappedInt(0), WrappedInt(1))) should equal(InfixFunctionColumn[Boolean]("in", TableOne.col7, ScalarFunctionColumn[WrappedInt]("", Seq(WrappedInt(0).constant, WrappedInt(1).constant))))
  }

  "column comparisons" should "fail for a mapped and an unmapped column" in {
    intercept[AssertionError] {
      TableOne.col1 > TableOne.col3
    }

    intercept[AssertionError] {
      TableOne.col3 =!= TableOne.col1
    }

    intercept[AssertionError] {
      1.column.between(TableOne.col3, 5)
    }
  }

  "optional mapped columns" should "work with non option comparisons correctly" in {
    TableOne.col2 === "abc"
    TableOne.col2 === Some("abc")
    TableOne.col5 === "abc"
    TableOne.col5 === Some("abc")
  }

  "between operator" should "fail when different mappings are applied to lower and upper" in {
    intercept[AssertionError] {
      1.column.between(TableOne.col3, 5)
    }
  }

  // it should "not allow === to apply to different base types" in {
  //   illTyped("""
  //     literalColumn(1) === "2"
  //   """)
  // }

}
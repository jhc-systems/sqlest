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

package sqlest.ast.syntax

import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.ast._

class CaseSyntaxSpec extends FlatSpec with Matchers {
  val zeroIsNoneColumnType = ZeroIsNoneColumnType[Int, Int]
  class MyTable(alias: Option[String]) extends Table("mytable", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[String]("col2")
    val col3 = column[Option[Int]]("col2")(zeroIsNoneColumnType)
  }
  object MyTable extends MyTable(None)

  "case statement" should "accept multiple whens" in {
    `case`(when(MyTable.col1 === 1, 2)) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2))))
    `case`(when(MyTable.col1 === 1, 2), when(MyTable.col2 === "abc", 3)) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3))))
  }

  "case statement" should "work as a builder" in {
    `case`().when(MyTable.col1 === 1, 2) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2))))

    `case`()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3))))

    `case`()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3)
      .`else`(5) should be(CaseWhenElseColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3)), 5))

    `case`()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3)
      .otherwise(5) should be(CaseWhenElseColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3)), 5))
  }

  "decode statement" should "accept multiple whens" in {
    decode(when(MyTable.col1 === 1, 2)) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2))))
    decode(when(MyTable.col1 === 1, 2), when(MyTable.col2 === "abc", 3)) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3))))
  }

  "decode statement" should "work as a builder" in {
    decode().when(MyTable.col1 === 1, 2) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2))))

    decode()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3) should be(CaseWhenColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3))))

    decode()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3)
      .`else`(5) should be(CaseWhenElseColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3)), 5))

    decode()
      .when(MyTable.col1 === 1, 2)
      .when(MyTable.col2 === "abc", 3)
      .otherwise(5) should be(CaseWhenElseColumn(List(When(MyTable.col1 === 1, 2), When(MyTable.col2 === "abc", 3)), 5))
  }

  "case column statement" should "work as a builder" in {
    `case`(MyTable.col1).when(1, 2) should be(
      CaseColumnColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2)))
    )

    `case`(MyTable.col1).when(1, 2).when(2, 3) should be(
      CaseColumnColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)))
    )

    `case`(MyTable.col1).when(1, 2).when(2, 3).`else`(4) should be(
      CaseColumnElseColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)), LiteralColumn(4))
    )

    `case`(MyTable.col1).when(1, 2).when(2, 3).otherwise(4) should be(
      CaseColumnElseColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)), LiteralColumn(4))
    )
  }

  "decode column statement" should "work as a builder" in {
    decode(MyTable.col1).when(1, 2) should be(
      CaseColumnColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2)))
    )

    decode(MyTable.col1).when(1, 2).when(2, 3) should be(
      CaseColumnColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)))
    )

    decode(MyTable.col1).when(1, 2).when(2, 3).`else`(4) should be(
      CaseColumnElseColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)), LiteralColumn(4))
    )

    decode(MyTable.col1).when(1, 2).when(2, 3).otherwise(4) should be(
      CaseColumnElseColumn(MyTable.col1, List(LiteralColumn(1) -> LiteralColumn(2), LiteralColumn(2) -> LiteralColumn(3)), LiteralColumn(4))
    )
  }

  "case statements" should "have columnType of the result column" in {
    `case`(when(MyTable.col1 === 1, MyTable.col3)).columnType should be(zeroIsNoneColumnType)
    `case`().when(MyTable.col1 === 1, MyTable.col3).columnType should be(zeroIsNoneColumnType)
    `case`(MyTable.col1).when(1, MyTable.col3).columnType should be(zeroIsNoneColumnType)
    decode(when(MyTable.col1 === 1, MyTable.col3)).columnType should be(zeroIsNoneColumnType)
    decode().when(MyTable.col1 === 1, MyTable.col3).columnType should be(zeroIsNoneColumnType)
    decode(MyTable.col1).when(1, MyTable.col3).columnType should be(zeroIsNoneColumnType)
  }
}
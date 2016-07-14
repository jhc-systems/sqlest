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

import org.joda.time.LocalDate
import org.scalatest._
import org.scalatest.matchers._

class MappedColumnTypeSpec extends FlatSpec with Matchers with MappedColumnTypes {
  "MappedBooleanColumnType" should "convert database values to booleans" in {
    val BooleanIntColumnType = MappedBooleanColumnType(1, 0)

    BooleanIntColumnType.write(true) should be(1)
    BooleanIntColumnType.write(false) should be(0)
    BooleanIntColumnType.read(Some(1)) should be(Some(true))
    BooleanIntColumnType.read(Some(0)) should be(Some(false))

    val BooleanStringColumnType = MappedBooleanColumnType("Y", "Z")

    BooleanStringColumnType.write(true) should be("Y")
    BooleanStringColumnType.write(false) should be("Z")
    BooleanStringColumnType.read(Some("Y")) should be(Some(true))
    BooleanStringColumnType.read(Some("Z")) should be(Some(false))
  }

  sealed trait Animal
  case object Snake extends Animal
  case object Gigantosaurus extends Animal
  case object Troglodyte extends Animal

  "EnumerationColumnType" should "convert database values to the enumeration" in {
    val AnimalColumnType = EnumerationColumnType(
      Snake -> "S",
      Gigantosaurus -> "G"
    )

    AnimalColumnType.write(Snake) should be("S")
    AnimalColumnType.write(Gigantosaurus) should be("G")

    intercept[NoSuchElementException] {
      AnimalColumnType.write(Troglodyte) should be("G")
    }
    AnimalColumnType.read(Some("S")) should be(Some(Snake))
    AnimalColumnType.read(Some("G")) should be(Some(Gigantosaurus))

    intercept[NoSuchElementException] {
      AnimalColumnType.read(Some("X")) should be(Some(Gigantosaurus))
    }
  }

  "EnumerationColumnType" should "accept a fallback value for reads using withDefault" in {
    val AnimalColumnType = EnumerationColumnType(
      Snake -> "S",
      Gigantosaurus -> "G"
    ).withDefault(Gigantosaurus)

    AnimalColumnType.write(Snake) should be("S")
    AnimalColumnType.write(Gigantosaurus) should be("G")

    intercept[NoSuchElementException] {
      AnimalColumnType.write(Troglodyte) should be("G")
    }

    AnimalColumnType.read(Some("S")) should be(Some(Snake))
    AnimalColumnType.read(Some("G")) should be(Some(Gigantosaurus))

    AnimalColumnType.read(Some("X")) should be(Some(Gigantosaurus))
  }

  it should "use a globally specified implicit value for its base column type" in {
    implicit val stringColumnType = TrimmedStringColumnType
    val AnimalColumnType = EnumerationColumnType(
      Snake -> "S",
      Gigantosaurus -> "G"
    )
    AnimalColumnType.read(Some("S   ")) shouldBe Some(Snake)
  }

  "OptionColumnType" should "convert zeroes in database to None" in {
    val zeroIsNoneIntColumnType = OptionColumnType[Int, Int](0)
    zeroIsNoneIntColumnType.write(Some(10)) should be(10)
    zeroIsNoneIntColumnType.write(None) should be(0)
    zeroIsNoneIntColumnType.read(Some(10)) should be(Some(Some(10)))
    zeroIsNoneIntColumnType.read(Some(0)) should be(Some(None))

    val zeroIsNoneBigDecimalColumnType = ZeroIsNoneColumnType[BigDecimal, BigDecimal]
    zeroIsNoneBigDecimalColumnType.write(Some(BigDecimal("3.1415"))) should be(BigDecimal("3.1415"))
    zeroIsNoneBigDecimalColumnType.write(None) should be(BigDecimal(0))
    zeroIsNoneBigDecimalColumnType.read(Some(BigDecimal("3.1415"))) should be(Some(Some(BigDecimal("3.1415"))))
    zeroIsNoneBigDecimalColumnType.read(Some(BigDecimal(0))) should be(Some(None))
  }

  "YyyyMmDdColumnType" should "convert integers to date times" in {
    YyyyMmDdColumnType.write(new LocalDate(1999, 12, 31)) should be(19991231)
    YyyyMmDdColumnType.write(new LocalDate(2000, 1, 1)) should be(20000101)
    YyyyMmDdColumnType.read(Some(19991231)) should be(Some(new LocalDate(1999, 12, 31)))
    YyyyMmDdColumnType.read(Some(20000101)) should be(Some(new LocalDate(2000, 1, 1)))
  }

  "MappedColumnType.compose" should "compose read and write operations" in {
    val zeroIsNoneStringColumn = OptionColumnType("0")(MappedColumnType[Int, String](_.toInt, _.toString))
    zeroIsNoneStringColumn.write(Some(1)) should be("1")
    zeroIsNoneStringColumn.write(None) should be("0")
    zeroIsNoneStringColumn.read(Some("0")) should be(Some(None))
    zeroIsNoneStringColumn.read(Some("1")) should be(Some(Some(1)))

    val blankIsNoneTrimmedColumn = OptionColumnType("")(TrimmedStringColumnType)
    blankIsNoneTrimmedColumn.read(Some("")) should be(Some(None))
    blankIsNoneTrimmedColumn.read(Some("  Test   ")) should be(Some(Some("  Test")))
    blankIsNoneTrimmedColumn.write(Some("Test")) should be("Test")
    blankIsNoneTrimmedColumn.write(None) should be("")

    val chainedComposedMappedColumn =
      YyyyMmDdColumnType.compose(
        MappedBooleanColumnType(
          new LocalDate(1999, 12, 31),
          new LocalDate(2000, 1, 1)
        ).compose(
            MappedColumnType[Option[Int], Boolean](
              db => if (db) Some(1) else None,
              v => if (v.getOrElse(0) == 0) false else true
            )
          )
      )

    chainedComposedMappedColumn.read(Some(19991231)) should be(Some(Some(1)))
    chainedComposedMappedColumn.read(Some(19900408)) should be(Some(None))
    chainedComposedMappedColumn.write(Some(5)) should be(19991231)
    chainedComposedMappedColumn.write(None) should be(20000101)
  }
}

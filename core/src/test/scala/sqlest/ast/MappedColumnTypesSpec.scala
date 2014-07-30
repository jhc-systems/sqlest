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

import org.joda.time.DateTime
import org.scalatest._
import org.scalatest.matchers._

class MappedColumnTypeSpec extends FlatSpec with Matchers with MappedColumnTypes {
  "MappedBooleanColumnType" should "convert database values to booleans" in {
    val BooleanIntColumnType = MappedBooleanColumnType(1, 0)

    BooleanIntColumnType.write(true) should be(1)
    BooleanIntColumnType.write(false) should be(0)
    BooleanIntColumnType.read(1) should be(true)
    BooleanIntColumnType.read(0) should be(false)

    val BooleanStringColumnType = MappedBooleanColumnType("Y", "Z")

    BooleanStringColumnType.write(true) should be("Y")
    BooleanStringColumnType.write(false) should be("Z")
    BooleanStringColumnType.read("Y") should be(true)
    BooleanStringColumnType.read("Z") should be(false)
  }

  sealed trait Animal
  case object Snake extends Animal
  case object Gigantosaurus extends Animal

  "EnumerationColumnType" should "convert database values to the enumeration" in {
    val AnimalColumnType = EnumerationColumnType(
      Snake -> "S",
      Gigantosaurus -> "G"
    )

    AnimalColumnType.write(Snake) should be("S")
    AnimalColumnType.write(Gigantosaurus) should be("G")
    AnimalColumnType.read("S") should be(Snake)
    AnimalColumnType.read("G") should be(Gigantosaurus)
  }

  "ZeroIsNoneColumnType" should "convert zeroes in database to None" in {
    ZeroIsNoneColumnType[Int].write(Some(10)) should be(10)
    ZeroIsNoneColumnType[Int].write(None) should be(0)
    ZeroIsNoneColumnType[Int].read(10) should be(Some(10))
    ZeroIsNoneColumnType[Int].read(0) should be(None)

    ZeroIsNoneColumnType[BigDecimal].write(Some(BigDecimal("3.1415"))) should be(BigDecimal("3.1415"))
    ZeroIsNoneColumnType[BigDecimal].write(None) should be(BigDecimal(0))
    ZeroIsNoneColumnType[BigDecimal].read(BigDecimal("3.1415")) should be(Some(BigDecimal("3.1415")))
    ZeroIsNoneColumnType[BigDecimal].read(BigDecimal(0)) should be(None)
  }

  "YyyyMmDdColumnType" should "convert integers to date times" in {
    YyyyMmDdColumnType.write(new DateTime(1999, 12, 31, 0, 0)) should be(19991231)
    YyyyMmDdColumnType.write(new DateTime(2000, 1, 1, 0, 0)) should be(20000101)
    YyyyMmDdColumnType.read(19991231) should be(new DateTime(1999, 12, 31, 0, 0))
    YyyyMmDdColumnType.read(20000101) should be(new DateTime(2000, 1, 1, 0, 0))
  }

}

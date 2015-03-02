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

class ColumnTypeSpec extends FlatSpec with Matchers {
  "basic data types" should "have implicit ColumnTypes" in {
    implicitly[ColumnType[Boolean]]
    implicitly[ColumnType[Int]]
    implicitly[ColumnType[Double]]
    implicitly[ColumnType[String]]
    implicitly[ColumnType[Array[Byte]]]
  }

  "optional data types" should "have implicit ColumnTypes" in {
    implicitly[ColumnType[Option[Boolean]]]
    implicitly[ColumnType[Option[Int]]]
    implicitly[ColumnType[Option[Double]]]
    implicitly[ColumnType[Option[String]]]
    implicitly[ColumnType[Option[Array[Byte]]]]
  }
}
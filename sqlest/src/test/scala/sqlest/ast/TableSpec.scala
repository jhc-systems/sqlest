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

class TableSpec extends FlatSpec with Matchers {
  class SimpleTable(alias: Option[String]) extends Table("mytable", alias)
  object SimpleTable extends SimpleTable(None)

  trait TableTrait
  class TraitedTable(alias: Option[String]) extends Table("traitedtable", alias) with TableTrait
  object TraitedTable extends TraitedTable(None)

  "as macro" should "alias table" in {
    val TheirTable = SimpleTable.as("their")
    TheirTable.tableAlias should be("their")
    TheirTable.as("your").tableAlias should be("your")
    new SimpleTable(Some("its")).as("mine").tableAlias should be("mine")
  }

  it should "also alias tables the implement other traits" in {
    val TheirTable = TraitedTable.as("their")
    TheirTable.tableAlias should be("their")
    TheirTable.as("your").tableAlias should be("your")
    new TraitedTable(Some("its")).as("mine").tableAlias should be("mine")
  }
}

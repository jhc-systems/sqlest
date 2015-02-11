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

class AggrgateFunctionSyntaxSpec extends FlatSpec with Matchers {
  class MyTable(alias: Option[String]) extends Table("mytable", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[String]("col2")
  }
  object MyTable extends MyTable(None)

  "aggregate functions" should "have default aliases" in {
    count().columnAlias should be("count")
    sum(MyTable.col1).columnAlias should be("sum")
    min(MyTable.col1).columnAlias should be("min")
    max(MyTable.col2).columnAlias should be("max")
    avg(MyTable.col1).columnAlias should be("avg")
  }
}
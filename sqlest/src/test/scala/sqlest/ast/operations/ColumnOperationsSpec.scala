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

package sqlest.ast.operations

import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.ast.operations.ColumnOperations._

class ColumnOperationsSpec extends FlatSpec with Matchers {

  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[Int]("col1")
  }
  object TableOne extends TableOne(None)

  "column.map(f)" should "map f over all internal columns" in {
    TableOne.col1.mapColumns(identity, identity) should be(TableOne.col1)
  }

  "select.mapColumns" should "map f over all internal columns" in {
    val tableOneSelect =
      select.from(TableOne)
        .where(exists(
          select(TableOne.col1)
            .from(TableOne)
        ))

    tableOneSelect.mapColumns(
      identity,
      _.where(TableOne.col1 =!= 2.constant)
    ) should be(
        select
          .from(TableOne)
          .where(exists(
            select(TableOne.col1)
              .from(TableOne)
              .where(TableOne.col1 =!= 2.constant)
          ))
      )
  }

}
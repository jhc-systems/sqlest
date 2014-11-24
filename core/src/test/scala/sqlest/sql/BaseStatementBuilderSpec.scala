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

package sqlest.sql

import org.scalatest._
import org.scalatest.matchers._
import sqlest._
import sqlest.ast._

trait BaseStatementBuilderSpec extends FlatSpec with Matchers {
  implicit class StringFormatOps(sql: String) {
    def formatSql = sql.trim.stripMargin.split(scala.util.Properties.lineSeparator).map(_.trim).mkString(" ")
  }
  implicit def statementBuilder: StatementBuilder

  def sql(operation: Operation) = {
    val preprocessedOperation = statementBuilder.preprocess(operation)
    (
      statementBuilder.sql(preprocessedOperation),
      statementBuilder.parameters(preprocessedOperation).map(_.value)
    )
  }

  // Test data ----------------------------------

  class MyTable(alias: Option[String]) extends Table("mytable", alias) {
    val col1 = column[Int]("col1")
    val col2 = column[Int]("col2")
  }
  object MyTable extends MyTable(None)

  class TableOne(alias: Option[String]) extends Table("one", alias) {
    val col1 = column[String]("col1")
    val col2 = column[String]("col2")
  }
  object TableOne extends TableOne(None) {
    implicit val tableTwoNaturalJoinCondition = NaturalJoinConditionBuilder {
      (tableOne: TableOne, tableTwo: TableTwo) => tableOne.col2 === tableTwo.col2
    }

    implicit val testTableFunctionJoinCondition = NaturalJoinConditionBuilder {
      (tableOne: TableOne, testTableFunction: TestTableFunction) => tableOne.col1 === testTableFunction.col6
    }
  }

  class TableTwo(alias: Option[String]) extends Table("two", alias) {
    val col2 = column[String]("col2")
    val col3 = column[String]("col3")
  }
  object TableTwo extends TableTwo(None)

  class TableThree(alias: Option[String]) extends Table("three", alias) {
    val col3 = column[Option[String]]("col3")
    val col4 = column[Option[String]]("col4")
  }
  object TableThree extends TableThree(None)

  sealed trait Medal
  case object Gold extends Medal
  case object Silver extends Medal
  case object Bronze extends Medal

  class TableFour(alias: Option[String]) extends Table("four", alias) {
    val orderedColumn = column[Medal]("orderedColumn", OrderedEnumerationColumnType[Medal, String](Gold -> "G", Silver -> "S", Bronze -> "B"))
  }
  object TableFour extends TableFour(None)

  class TestTableFunction(alias: Option[String]) extends TableFunction2[String, String]("testTableFunction", alias) {
    val col5 = column[String]("col5")
    val col6 = column[String]("col6")
  }
  object TestTableFunction extends TestTableFunction(None)

  val testFunction = ScalarFunction2[String, String, Int]("testFunction")
}

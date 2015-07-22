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

import java.sql.ResultSet
import sqlest.ast._
import sqlest.ast.syntax._
import sqlest.executor._
import sqlest.extractor._

package object sqlest
    // sqlest.ast
    extends MappedColumnTypes
    with ScalarFunctions
    with TableFunctions
    with TupleGroups
    // sqlest.ast.syntax
    with QuerySyntax
    with ColumnSyntax
    with JoinSyntax
    with OrderSyntax
    with GroupSyntax
    with CaseSyntax
    with ScalarFunctionSyntax
    with AggregateFunctionSyntax
    with OlapFunctionSyntax
    with UntypedColumnSyntax
    // sqlest.extractor
    with ExtractorSyntax[ResultSet]
    with ColumnExtractorSyntax
    with ColumnExtractorSetters
    // sqlest.executor
    with ExecutorSyntax {
  type Table = sqlest.ast.Table

  type StatementBuilder = sqlest.sql.base.StatementBuilder

  type Database = sqlest.executor.Database
  val Database = sqlest.executor.Database

  type Session = sqlest.executor.Session
  type Transaction = sqlest.executor.Transaction

  type Extractor[A] = sqlest.extractor.Extractor[ResultSet, A]
}

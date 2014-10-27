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

import sqlest.ast._
import sqlest.ast.operations._
import sqlest.ast.syntax._
import sqlest.executor._
import sqlest.extractor._
import sqlest.untyped._

package object sqlest extends SqlestCore
  with QuerySyntax
  with ColumnSyntax
  with JoinSyntax
  with OrderSyntax
  with ExtractorSyntax
  with ExecutorSyntax
  with MappedColumnTypes
  with TableFunctions
  with ScalarFunctions
  with AggregateFunctionSyntax
  with OlapFunctionSyntax
  with CaseSyntax
  with ScalarFunctionSyntax
  with GroupSyntax
  with TupleGroups
  with SqlestUntyped

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

import sqlest.ast.{ Column, GroupFunctionColumn }

trait H2StatementBuilder extends StatementBuilder {
  override def columnSql(column: Column[_]): String = column match {
    case column: GroupFunctionColumn[_] => throw new UnsupportedOperationException
    case column => super.columnSql(column)
  }
}

object H2StatementBuilder extends H2StatementBuilder

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

import sqlest.ast._

trait H2StatementBuilder extends base.StatementBuilder {
  override def groupSql(group: Group, relation: Relation): String = group match {
    case group: FunctionGroup => throw new UnsupportedOperationException
    case group => super.groupSql(group, relation)
  }
}

object H2StatementBuilder extends H2StatementBuilder

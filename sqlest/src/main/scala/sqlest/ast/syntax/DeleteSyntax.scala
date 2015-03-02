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

import sqlest.ast._

trait DeleteSyntax {
  /** Delete from a relation: `delete.from(...)`. */
  def from(from: Table) =
    new DeleteBuilder(from)
}

/** Helper class to prevent users writing `delete.from(...)` without `.where(...)`. */
class DeleteBuilder(from: Table) {
  def where(where: Column[Boolean]) =
    Delete(from = from, where = Some(where))

  def allRows = Delete(from = from, where = None)
}

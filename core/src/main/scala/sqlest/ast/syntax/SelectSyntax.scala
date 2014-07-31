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

trait SelectSyntax {
  /** Select all columns from a relation: `select.from(...)`. */
  def from(from: Relation) = Select(Nil, from = from)

  /** Select a subset of columns from a relation: `select(...).from(...)`. */
  def apply(what: AliasedColumn[_]*) = new SelectBuilder(what.toSeq)

  /** Select a subset of columns from a relation: `select(extractor.columns).from(...)`. */
  def apply(what: List[AliasedColumn[_]]) = new SelectBuilder(what)
}

/** Helper class to enable the `select(...).from(...)` syntax. */
class SelectBuilder(what: Seq[AliasedColumn[_]]) {
  def from(from: Relation) = Select(what = what, from = from)
}

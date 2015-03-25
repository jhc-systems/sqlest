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

trait InsertSyntax {
  /** Insert into a relation: `insert.into(...)`. */
  def into(into: Table) =
    new InsertBuilder(into)
}

/** Helper class to prevent users writing `insert.into(...)` without `.columns(...).values(...)` or `.set(...)`. */
class InsertBuilder(into: Table) {
  def columns(columns: TableColumn[_]*) =
    new InsertColumnsBuilder(into, columns)

  def values(setters: Setter[_, _]*) =
    InsertValues(into, Seq(setters))

  def values(setters: => Seq[Setter[_, _]]) =
    InsertValues(into, Seq(setters))

  def set(setters: Setter[_, _]*) =
    InsertValues(into, Seq(setters))

  def set(setters: => Seq[Setter[_, _]]) =
    InsertValues(into, Seq(setters))
}

/** Helper class to prevent users writing `insert.into(...).columns(...)` without `.values(...)` */
class InsertColumnsBuilder(into: Table, columns: Seq[TableColumn[_]]) {
  def values(setters: Setter[_, _]*) = {
    if (columns != setters.map(_.column)) throw new AssertionError(s"Cannot insert value to the columns declared")
    InsertValues(into, Seq(setters))
  }

  def values(setters: => Seq[Setter[_, _]]) = {
    if (columns != setters.map(_.column)) throw new AssertionError(s"Cannot insert value to the columns declared")
    InsertValues(into, Seq(setters))
  }

  def from[A: AliasedColumns](select: Select[A, _ <: Relation]) =
    InsertFromSelect(into = into, columns = columns, select = select)
}

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

package sqlest.ast

import sqlest.ast.syntax._

case class Merge[R <: Relation](
    into: (Table, String),
    using: (Select[_, R], String),
    condition: Column[Boolean],
    whenMatched: Option[MatchedOp] = None,
    whenMatchedAnd: List[MatchedAndOp] = List(),
    whenNotMatched: Option[NotMatchedOp] = None,
    whenNotMatchedAnd: List[NotMatchedAndOp] = List()
) extends Command with ColumnSyntax {
  def whenMatched(op: MatchedOp): Merge[R] = this.copy(whenMatched = this.whenMatched.map(c => c).orElse(Some(op)))
  def whenMatchedAnd(ops: MatchedAndOp): Merge[R] = this.copy(whenMatchedAnd = ops :: this.whenMatchedAnd)
  def whenMatchedAnd(ops: List[MatchedAndOp]): Merge[R] = this.copy(whenMatchedAnd = this.whenMatchedAnd ::: ops)
  def whenNotMatched(op: NotMatchedOp): Merge[R] = this.copy(whenNotMatched = this.whenNotMatched.map(c => c).orElse(Some(op)))
  def whenNotMatchedAnd(ops: NotMatchedAndOp): Merge[R] = this.copy(whenNotMatchedAnd = ops :: this.whenNotMatchedAnd)
  def whenNotMatchedAnd(ops: List[NotMatchedAndOp]): Merge[R] = this.copy(whenNotMatchedAnd = this.whenNotMatchedAnd ::: ops)
}

trait MergeOperation

case class MatchedOp(op: Either[Update, String]) extends MergeOperation
case class MatchedAndOp(op: Either[Update, String], and: Column[Boolean]) extends MergeOperation
case class NotMatchedOp(op: Insert) extends MergeOperation
case class NotMatchedAndOp(op: Insert, and: Column[Boolean]) extends MergeOperation
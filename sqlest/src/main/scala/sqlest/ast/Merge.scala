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
    whenMatched: Option[Either[Update, DeleteSyntax]] = None,
    whenMatchedAnd: List[(Either[Update, DeleteSyntax], Column[Boolean])] = List(),
    whenNotMatched: Option[Insert] = None,
    whenNotMatchedAnd: List[(Insert, Column[Boolean])] = List()
) extends Command with ColumnSyntax {

  def whenMatched(op: Update): MergeMatch[R] = MergeMatch(this, MatchedOp(Left(op)))
  def whenMatched(op: DeleteSyntax): MergeMatch[R] = MergeMatch(this, MatchedOp(Right(op)))
  def whenNotMatched(op: Insert): MergeMatch[R] = MergeMatch(this, NotMatchedOp(op))
}

case class MergeMatch[R <: Relation](merge: Merge[R], op: MergeOperation) {
  def and(columnBoolean: Option[Column[Boolean]]): Merge[R] = {
    op match {
      case op: MatchedOp => columnBoolean.map(and =>
        merge.copy(whenMatchedAnd = (op.op, and) :: merge.whenMatchedAnd))
        .getOrElse(merge.copy(whenMatched = merge.whenMatched.map(c => c).orElse(Some(op.op))))
      case op: NotMatchedOp => columnBoolean.map(and =>
        merge.copy(whenNotMatchedAnd = (op.op, and) :: merge.whenNotMatchedAnd))
        .getOrElse(merge.copy(whenNotMatched = merge.whenNotMatched.map(c => c).orElse(Some(op.op))))
    }

  }
}

trait MergeOperation
case class MatchedOp(op: Either[Update, DeleteSyntax]) extends MergeOperation
case class NotMatchedOp(op: Insert) extends MergeOperation

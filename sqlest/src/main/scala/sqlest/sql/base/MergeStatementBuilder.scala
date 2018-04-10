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

package sqlest.sql.base

import sqlest.ast._

trait MergeStatementBuilder extends BaseStatementBuilder {
  type MergeType = Merge[_ <: Relation]
  def mergeSql(merge: MergeType, usingSelect: String, matchedOps: List[(String, String)], notMatchedOp: List[(String, String)]): String = {
    s"merge ${mergeIntoSql(merge.into)} " +
      s"using ($usingSelect) as ${merge.using._2} " +
      s"${mergeOnSql(merge.condition.get)} " +
      s"${mergeMatchedSql(matchedOps)} " +
      s"${mergeNotMatchedSql(notMatchedOp)}"
  }

  def mergeIntoSql(into: (Table, String)): String =
    s"into ${identifierSql(into._1.tableName)} as ${into._2}"

  def mergeOnSql(condition: Column[Boolean]): String =
    s"on ${columnSql(condition)}"

  def mergeMatchedSql(op: List[(String, String)]): String = op.map {
    case (and, s) =>
      s"when matched $and then $s"
  }.mkString("\n")

  def mergeNotMatchedSql(op: List[(String, String)]): String = op.map {
    case (and, s) =>
      s"when not matched $and then $s"
  }.mkString("\n")

}

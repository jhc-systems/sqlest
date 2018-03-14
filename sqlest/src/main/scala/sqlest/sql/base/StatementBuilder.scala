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

trait StatementBuilder extends BaseStatementBuilder
    with SelectStatementBuilder
    with InsertStatementBuilder
    with UpdateStatementBuilder
    with DeleteStatementBuilder
    with MergeStatementBuilder {

  def apply(operation: Operation) = {
    val preprocessedOperation = preprocess(operation)
    (preprocessedOperation, sql(preprocessedOperation), argumentLists(preprocessedOperation))
  }

  def generateRawSql(operation: Operation): String = {
    val preprocessedOperation = preprocess(operation)
    val querySql = sql(preprocessedOperation).split("\\?")
    val queryArguments = argumentLists(preprocessedOperation).flatten.map(argument => constantSql(argument.columnType.asInstanceOf[ColumnType[Any]], argument.value))

    querySql.zipAll(queryArguments, "", "")
      .map { case (sql, argument) => sql + argument }
      .mkString
  }

  private def sql(operation: Operation): String = operation match {
    case select: Select[_, _] => selectSql(select)
    case insert: Insert => insertSql(insert)
    case update: Update => updateSql(update)
    case delete: Delete => deleteSql(delete)
    case merge: Merge[_] =>
      val whenMatched = merge.whenMatched.map {
        case MatchedOp(Left(updateOp)) => ("", updateSql(updateOp))
        case MatchedOp(Right(_)) => ("", "DELETE")
      }
      val whenMatchedAnd = merge.whenMatchedAnd.map {
        case MatchedAndOp(Left(updateOp), and) => ("and " + columnSql(and), updateSql(updateOp))
        case MatchedAndOp(Right(_), and) => ("and " + columnSql(and), "DELETE")
      }
      val whenNotMatchedAnd = merge.whenNotMatchedAnd.map {
        case NotMatchedAndOp(insertOp, and) => ("and " + columnSql(and), insertSql(insertOp))
      }
      val whenNotMatched = merge.whenNotMatched.map(op => ("", insertSql(op.op)))
      merge.using match {
        case s: Select[_, _] =>
          mergeSql(merge, selectSql(s), whenMatched.map(s => s :: whenMatchedAnd).getOrElse(whenMatchedAnd), whenNotMatched.map(s => s :: whenNotMatchedAnd).getOrElse(whenNotMatchedAnd))
        case _@ errorType => sys.error("Unsupported merge select type: " + errorType)
      }
    case other => sys.error("Unsupported operation type: " + other)
  }

  private def argumentLists(operation: Operation): List[List[LiteralColumn[_]]] = operation match {
    case select: Select[_, _] => List(selectArgs(select))
    case insert: Insert => insertArgs(insert)
    case update: Update => List(updateArgs(update))
    case delete: Delete => List(deleteArgs(delete))
    case merge: Merge[_] => merge.whenMatched.map {
      case MatchedOp(Left(updateOp)) => updateArgs(updateOp)
      case MatchedOp(Right(_)) => List()
    }.getOrElse(List()) ::
      merge.whenMatchedAnd.map {
        case MatchedAndOp(Left(updateOp), and) => updateArgs(updateOp)
        case MatchedAndOp(Right(_), and) => List()
      } :::
      merge.whenNotMatched.map(op => insertArgs(op.op)).getOrElse(List())
    case other => sys.error("Unsupported operation type: " + other)
  }

}

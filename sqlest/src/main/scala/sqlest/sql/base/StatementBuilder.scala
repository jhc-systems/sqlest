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
        case MatchedOp(Left(updateOp)) => ("", s"UPDATE ${updateSetSql(updateOp.set)}")
        case MatchedOp(Right(_)) => ("", "DELETE")
      }
      val whenMatchedAnd = merge.whenMatchedAnd.map {
        case MatchedAndOp(Left(updateOp), and) => (s"AND ${columnSql(and)}", s"UPDATE ${updateSetSql(updateOp.set)}")
        case MatchedAndOp(Right(_), and) => (s"AND ${columnSql(and)}", "DELETE")
      }
      val whenNotMatchedAnd = merge.whenNotMatchedAnd.map {
        case NotMatchedAndOp(insertValues: InsertValues, and) => (s"AND ${columnSql(and)}", s"INSERT ${insertColumnsSql(insertValues.columns)} ${insertValuesSql(insertValues.columns)}}")
        case NotMatchedAndOp(InsertFromSelect(into, columns, select), and) => (s"AND ${columnSql(and)}", s"INSERT ${insertColumnsSql(columns)} ${selectSql(select)}")
      }
      val whenNotMatched = merge.whenNotMatched.map {
        case NotMatchedOp(insertValues: InsertValues) => ("", s"INSERT ${insertColumnsSql(insertValues.columns)} ${insertValuesSql(insertValues.columns)}")
        case NotMatchedOp(InsertFromSelect(into, columns, select)) => ("", s"INSERT ${insertColumnsSql(columns)} ${selectSql(select)}")
      }
      merge.using match {
        case (s: Select[_, _], _) =>
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
    case merge: Merge[_] => {
      val whenMatched = merge.whenMatched.flatMap {
        case MatchedOp(Left(updateOp)) => Some(updateArgs(updateOp))
        case MatchedOp(Right(_)) => None
      }
      val whenMatchedAnd = merge.whenMatchedAnd.flatMap {
        case MatchedAndOp(Left(updateOp), and) => updateArgs(updateOp)
        case MatchedAndOp(Right(_), and) => Nil
      }
      val whenNotMatched = merge.whenNotMatched.map(op => insertArgs(op.op).flatten)
      val whenNotMatchedAnd = merge.whenNotMatchedAnd.flatMap {
        case NotMatchedAndOp(insert, and) => insertArgs(insert).flatten
      }
      (whenMatched, whenNotMatched, whenMatchedAnd, whenNotMatchedAnd) match {
        case (None, None, Nil, Nil) => Nil
        case (Some(wm), None, Nil, Nil) => List(wm)
        case (Some(wm), None, Nil, wnma) => List(wnma)
        case (None, Some(wnm), Nil, Nil) => List(wnm)
        case (None, Some(wnm), Nil, wnma) => List(wnm ::: wnma)
        case (Some(wm), Some(wnm), Nil, Nil) => List(wm ::: wnm)
        case (Some(wm), Some(wnm), Nil, wnma) => List(wm ::: wnm ::: wnma)
        case (Some(wm), Some(wnm), wma, Nil) => List(wm ::: wma ::: wnm)
        case (Some(wm), Some(wnm), wma, wnma) => List(wm ::: wma ::: wnm ::: wnma)
        case (None, Some(wnm), wma, Nil) => List(wnm ::: wma)
        case (None, Some(wnm), wma, wnma) => List(wnm ::: wma ::: wnma)
        case (Some(wm), None, wma, Nil) => List(wm ::: wma)
        case (Some(wm), None, wma, wnma) => List(wm ::: wma ::: wnma)
        case (None, None, wma, Nil) => List(wma)
        case (None, None, wma, wnma) => List(wma ::: wnma)
        case _ => Nil
      }
    }
    case other => sys.error("Unsupported operation type: " + other)
  }

}

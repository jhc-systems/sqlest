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
    with DeleteStatementBuilder {

  def apply(operation: Operation) = {
    val preprocessedOperation = preprocess(operation)
    val rawSql = sql(preprocessedOperation)
    val argLists = argumentLists(preprocessedOperation)
    (preprocessedOperation, rawSql, argLists, prettySql(rawSql, argLists))
  }

  def generateRawSql(operation: Operation): String = {
    val preprocessedOperation = preprocess(operation)
    val querySql = sql(preprocessedOperation).split("\\?")
    val queryArguments = argumentLists(preprocessedOperation).flatten.map(argument => constantSql(argument.columnType.asInstanceOf[ColumnType[Any]], argument.value))

    querySql.zipAll(queryArguments, "", "")
      .map { case (sql, argument) => sql + argument }
      .mkString
  }

  private def prettySql(sql: String, argumentLists: List[List[LiteralColumn[_]]]): String = {
    def literals(argList: List[LiteralColumn[_]]) = argList.map {
      case literal: LiteralColumn[a] =>
        constantSql(literal.columnType, literal.value)
    }

    val frags = sql.split("\\?")
    val fragLen = frags.length

    val firstArgs = argumentLists
      .headOption
      .toList
      .flatMap(literals)

    val firstArgsSql = frags.zipAll(firstArgs, "", "").flatMap {
      case (l, r) => Seq(l, r)
    }.mkString

    val restArgLists =
      if (firstArgs.isEmpty) Nil
      else argumentLists
        .drop(1)
        .flatMap(literals)
        .grouped(firstArgs.length)
        .toList

    val restArgListsSql = restArgLists.map { argList =>
      withLineBreaks(argList, 7)("(", ", ", ")")
    }

    val restArgsSql =
      if (restArgListsSql.isEmpty)
        ""
      else restArgListsSql.mkString(
        "," + NewLine + padding(7),
        "," + NewLine + padding(7),
        ""
      )

    if (restArgsSql.isEmpty)
      firstArgsSql
    else
      firstArgsSql + restArgsSql
  }

  private def sql(operation: Operation): String = operation match {
    case select: Select[_, _] => selectSql(select, 0)
    case insert: Insert => insertSql(insert, 0)
    case update: Update => updateSql(update, 0)
    case delete: Delete => deleteSql(delete, 0)
    case other => sys.error("Unsupported operation type: " + other)
  }

  private def argumentLists(operation: Operation): List[List[LiteralColumn[_]]] = operation match {
    case select: Select[_, _] => List(selectArgs(select))
    case insert: Insert => insertArgs(insert)
    case update: Update => List(updateArgs(update))
    case delete: Delete => List(deleteArgs(delete))
    case other => sys.error("Unsupported operation type: " + other)
  }

}

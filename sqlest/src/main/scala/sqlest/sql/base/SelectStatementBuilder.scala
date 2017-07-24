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

trait SelectStatementBuilder extends BaseStatementBuilder {
  def selectSql(select: Select[_, _ <: Relation], indent: Int): String = {
    Seq(
      selectWhatSql(select.columns, indent),
      selectFromSql(select.from, indent)
    ) ++ Seq(
        selectWhereSql(select.where, indent),
        selectStartWithSql(select.startWith, indent),
        selectConnectBySql(select.connectBy, indent),
        selectGroupBySql(select.groupBy, indent),
        selectHavingSql(select.having, indent),
        selectOrderBySql(select.orderBy, indent),
        selectLimitSql(select.limit),
        selectOffsetSql(select.offset),
        selectUnionSql(select.union, indent)
      ).flatten mkString (NewLine + padding(indent))
  }

  def selectWhatSql(columns: Seq[Column[_]], indent: Int): String =
    s"select ${columnAliasListSql(columns, indent + TabWidth)}"

  def selectFromSql(from: Relation, indent: Int): String =
    s"from ${joinSql(from, indent)}"

  def selectWhereSql(where: Option[Column[Boolean]], indent: Int): Option[String] =
    where map (where => s"where ${columnSql(where, indent)}")

  def selectStartWithSql(startWith: Option[Column[Boolean]], indent: Int): Option[String] =
    startWith map (startWith => s"start with ${columnSql(startWith, indent)}")

  def selectConnectBySql(connectBy: Option[Column[Boolean]], indent: Int): Option[String] =
    connectBy map (connectBy => s"connect by ${columnSql(connectBy, indent)}")

  def selectGroupBySql(group: Seq[Group], indent: Int): Option[String] =
    if (group.isEmpty) None else Some(s"group by ${groupListSql(group, indent)}")

  def selectHavingSql(having: Option[Column[Boolean]], indent: Int): Option[String] =
    having map (having => s"having ${columnSql(having, indent)}")

  def selectOrderBySql(order: Seq[Order], indent: Int): Option[String] =
    if (order.isEmpty) None else Some(s"order by ${orderListSql(order, indent)}")

  def selectLimitSql(limit: Option[Long]): Option[String] =
    limit map (limit => s"limit ${literalSql(limit)}")

  def selectOffsetSql(offset: Option[Long]): Option[String] =
    offset map (offset => s"offset ${literalSql(offset)}")

  def selectUnionSql(union: Seq[Union[_]], indent: Int): Option[String] =
    if (union.isEmpty)
      None
    else
      Some(union.map {
        case Union(select, false) => s"union" + onNewLine(selectSql(select, indent), indent)
        case Union(select, true) => s"union all" + onNewLine(selectSql(select, indent), indent)
      }.mkString(NewLine + padding(indent)))

  def joinSql(relation: Relation, indent: Int): String = relation match {
    case table: Table if table.tableName == table.tableAlias => identifierSql(table.tableName)
    case table: Table if table.tableName != table.tableAlias => identifierSql(table.tableName) + " as " + identifierSql(table.tableAlias)
    case tableFunctionApplication: TableFunctionApplication[_] => functionSql(tableFunctionApplication.tableName, tableFunctionApplication.parameterColumns, indent) + " as " + identifierSql(tableFunctionApplication.tableAlias)
    case TableFunctionFromSelect(select, alias) =>
      throw new UnsupportedOperationException
    case LeftJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("left join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case LeftExceptionJoin(left, right, condition) =>
      throw new UnsupportedOperationException
    case RightJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("right join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case RightExceptionJoin(left, right, condition) =>
      throw new UnsupportedOperationException
    case InnerJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("inner join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case OuterJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("full outer join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case CrossJoin(left, right) =>
      joinSql(left, indent) +
        onNewLine("cross join ", indent) +
        joinSql(right, indent)
    case select: Select[_, _] =>
      subselectSql(select, indent)
    case Lateral(select: Select[_, _]) =>
      "lateral " + subselectSql(select, indent)
  }

  def subselectSql(select: Select[_, _ <: Relation], indent: Int) = {
    val alias =
      if (select.subselectAlias.isDefined) " as " + select.subselectAlias.get
      else ""

    "(" +
      onNewLine(selectSql(select, indent + TabWidth), indent + TabWidth) +
      onNewLine(")" + alias, indent)
  }

  // -------------------------------------------------

  def selectArgs(select: Select[_, _ <: Relation]): List[LiteralColumn[_]] = {
    selectWhatArgs(select.columns) ++
      selectFromArgs(select.from) ++
      selectWhereArgs(select.where) ++
      selectStartWithArgs(select.startWith) ++
      selectConnectByArgs(select.connectBy) ++
      selectGroupByArgs(select.groupBy) ++
      selectHavingArgs(select.having) ++
      selectOrderByArgs(select.orderBy) ++
      selectLimitArgs(select.limit) ++
      selectOffsetArgs(select.offset) ++
      selectUnionArgs(select.union)
  }

  def selectWhatArgs(columns: Seq[Column[_]]): List[LiteralColumn[_]] =
    columnAliasListArgs(columns)

  def selectFromArgs(from: Relation): List[LiteralColumn[_]] =
    joinArgs(from)

  def selectWhereArgs(where: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    where map columnArgs getOrElse Nil

  def selectStartWithArgs(startWith: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    startWith map columnArgs getOrElse Nil

  def selectConnectByArgs(connectBy: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    connectBy map columnArgs getOrElse Nil

  def selectGroupByArgs(group: Seq[Group]): List[LiteralColumn[_]] =
    group.toList flatMap {
      case ColumnGroup(column) => columnArgs(column)
      case TupleGroup(groups) => selectGroupByArgs(groups)
      case FunctionGroup(_, groups) => selectGroupByArgs(groups)
    }

  def selectHavingArgs(having: Option[Column[Boolean]]): List[LiteralColumn[_]] =
    having map columnArgs getOrElse Nil

  def selectOrderByArgs(order: Seq[Order]): List[LiteralColumn[_]] =
    order.toList.map(_.column) flatMap columnArgs

  def selectLimitArgs(limit: Option[Long]): List[LiteralColumn[_]] =
    limit.map(LiteralColumn[Long](_)).toList

  def selectOffsetArgs(offset: Option[Long]): List[LiteralColumn[_]] =
    offset.map(LiteralColumn[Long](_)).toList

  def selectUnionArgs(union: Seq[Union[_]]): List[LiteralColumn[_]] =
    union.toList.map(_.select) flatMap selectArgs

  def joinArgs(relation: Relation): List[LiteralColumn[_]] = relation match {
    case table: Table => Nil
    case tableFunctionApplication: TableFunctionApplication[_] => tableFunctionApplication.parameterColumns.toList.flatMap(columnArgs)
    case tableFunction: TableFunctionFromSelect[_, _] => selectArgs(tableFunction.select)
    case LeftJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case LeftExceptionJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case RightJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case RightExceptionJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case InnerJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case OuterJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case CrossJoin(left, right) => joinArgs(left) ++ joinArgs(right)
    case select: Select[_, _] => selectArgs(select)
    case Lateral(select: Select[_, _]) => selectArgs(select)
  }
}

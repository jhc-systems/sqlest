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
  def selectSql(select: Select[_, _ <: Relation]): String = {
    Seq(
      selectWhatSql(select.columns),
      selectFromSql(select.from)
    ) ++ Seq(
        selectWhereSql(select.where),
        selectStartWithSql(select.startWith),
        selectConnectBySql(select.connectBy),
        selectGroupBySql(select.groupBy),
        selectHavingSql(select.having),
        selectOrderBySql(select.orderBy),
        selectLimitSql(select.limit),
        selectOffsetSql(select.offset),
        selectUnionSql(select.union)
      ).flatten mkString (" ")
  }

  def selectWhatSql(columns: Seq[Column[_]]): String =
    s"select ${columnAliasListSql(columns)}"

  def selectFromSql(from: Relation): String =
    s"from ${joinSql(from)}"

  def selectWhereSql(where: Option[Column[Boolean]]): Option[String] =
    where map (where => s"where ${columnSql(where)}")

  def selectStartWithSql(startWith: Option[Column[Boolean]]): Option[String] =
    startWith map (startWith => s"start with ${columnSql(startWith)}")

  def selectConnectBySql(connectBy: Option[Column[Boolean]]): Option[String] =
    connectBy map (connectBy => s"connect by ${columnSql(connectBy)}")

  def selectGroupBySql(group: Seq[Group]): Option[String] =
    if (group.isEmpty) None else Some(s"group by ${groupListSql(group)}")

  def selectHavingSql(having: Option[Column[Boolean]]): Option[String] =
    having map (having => s"having ${columnSql(having)}")

  def selectOrderBySql(order: Seq[Order]): Option[String] =
    if (order.isEmpty) None else Some(s"order by ${orderListSql(order)}")

  def selectLimitSql(limit: Option[Long]): Option[String] =
    limit map (limit => s"limit ${literalSql(limit)}")

  def selectOffsetSql(offset: Option[Long]): Option[String] =
    offset map (offset => s"offset ${literalSql(offset)}")

  def selectUnionSql(union: Seq[Union[_]]): Option[String] =
    if (union.isEmpty) None else
      Some(union.map {
        _ match {
          case Union(select, false) => s"union ${selectSql(select)}"
          case Union(select, true) => s"union all ${selectSql(select)}"
        }
      }.mkString(" "))

  def joinSql(relation: Relation): String = relation match {
    case table: Table if table.tableName == table.tableAlias => identifierSql(table.tableName)
    case table: Table if table.tableName != table.tableAlias => identifierSql(table.tableName) + " as " + identifierSql(table.tableAlias)
    case tableFunctionApplication: TableFunctionApplication[_] => functionSql(tableFunctionApplication.tableName, tableFunctionApplication.parameterColumns) + " as " + identifierSql(tableFunctionApplication.tableAlias)
    case LeftJoin(left, right, condition) => joinSql(left) + " left join " + joinSql(right) + " on " + columnSql(condition)
    case RightJoin(left, right, condition) => joinSql(left) + " right join " + joinSql(right) + " on " + columnSql(condition)
    case InnerJoin(left, right, condition) => joinSql(left) + " inner join " + joinSql(right) + " on " + columnSql(condition)
    case OuterJoin(left, right, condition) => joinSql(left) + " full outer join " + joinSql(right) + " on " + columnSql(condition)
    case CrossJoin(left, right) => joinSql(left) + " cross join " + joinSql(right)
    case select: Select[_, _] => subselectSql(select)
  }

  def subselectSql(select: Select[_, _ <: Relation]) = {
    val alias =
      if (select.subselectAlias.isDefined) " as " + select.subselectAlias.get
      else ""

    "(" + selectSql(select) + ")" + alias
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
      _ match {
        case ColumnGroup(column) => columnArgs(column)
        case TupleGroup(groups) => selectGroupByArgs(groups)
        case FunctionGroup(_, groups) => selectGroupByArgs(groups)
      }
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
    case LeftJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case RightJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case InnerJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case OuterJoin(left, right, condition) => joinArgs(left) ++ joinArgs(right) ++ columnArgs(condition)
    case CrossJoin(left, right) => joinArgs(left) ++ joinArgs(right)
    case select: Select[_, _] => selectArgs(select)
  }
}

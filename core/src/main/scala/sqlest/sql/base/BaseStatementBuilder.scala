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

trait BaseStatementBuilder {
  def columnAliasListSql(columns: Seq[Column[_]], relation: Relation): String =
    columns.map(column => columnAliasSql(column, relation)).mkString(", ")

  def columnAliasSql(column: Column[_], relation: Relation): String = column match {
    case column: TableColumn[_] =>
      columnSql(column.column, relation) + " as " +
        identifierSql(column.columnAlias)

    case column: AliasColumn[_] =>
      columnSql(column.column, relation) + " as " +
        identifierSql(column.columnAlias)

    case column =>
      columnSql(column, relation)
  }

  def columnSql(column: Column[_], relation: Relation): String =
    findColumnInRelation(column, relation).getOrElse(column match {
      case LiteralColumn(literal) => literalSql(literal)
      case column: ConstantColumn[_] => constantSql(column.columnType, column.value)
      case column: PrefixFunctionColumn[_] => prefixSql(column.name, column.parameter, relation)
      case column: InfixFunctionColumn[_] => infixSql(column.name, column.parameter1, column.parameter2, relation)
      case column: PostfixFunctionColumn[_] => postfixSql(column.name, column.parameter, relation)
      case column: DoubleInfixFunctionColumn[_] => doubleInfixSql(column.infix1, column.infix2, column.parameter1, column.parameter2, column.parameter3, relation)
      case SelectColumn(select) => "(" + selectSql(select) + ")"
      case WindowFunctionColumn(partitionByColumns, orders) => windowFunctionSql(partitionByColumns, orders, relation)
      case column: ScalarFunctionColumn[_] => functionSql(column.name, column.parameters, relation)
      case column: TableColumn[_] => identifierSql(column.tableAlias) + "." + identifierSql(column.columnName)
      case column: AliasColumn[_] => columnSql(column.column, relation)
      case column: CaseWhenColumn[_] => caseSql(column.whens, None, relation)
      case column: CaseWhenElseColumn[_] => caseSql(column.whens, Some(column.`else`), relation)
      case column: CaseColumnColumn[_, _] => caseColumnSql(column.column, column.mappings, None, relation)
      case column: CaseColumnElseColumn[_, _] => caseColumnSql(column.column, column.mappings, Some(column.`else`), relation)
    })

  def findColumnInRelation(column: Column[_], relation: Relation): Option[String] = relation match {
    case join: Join => findColumnInRelation(column, join.left) orElse findColumnInRelation(column, join.right)
    case select: Select[_] => findColumnInSubselectColumns(column, select.columns) orElse findColumnInSubselect(column, select.from)
    case _ => None
  }

  def findColumnInSubselectColumns(column: Column[_], subselectColumns: Seq[AliasedColumn[_]]): Option[String] =
    subselectColumns
      .find(subselectColumn => subselectColumn == column)
      .map(_.columnAlias)

  def findColumnInSubselect(column: Column[_], relation: Relation): Option[String] = relation match {
    case table: Table =>
      column match {
        case tableColumn: TableColumn[_] if (tableColumn.tableAlias == table.tableAlias) => Some(tableColumn.columnAlias)
        case _ => None
      }
    case tableFunction: TableFunction =>
      column match {
        case tableColumn: TableColumn[_] if (tableColumn.tableAlias == tableFunction.tableAlias) => Some(tableColumn.columnAlias)
        case _ => None
      }
    case join: Join => findColumnInSubselect(column, join.left) orElse findColumnInRelation(column, join.right)
    case select: Select[_] => findColumnInSubselectColumns(column, select.columns) orElse findColumnInSubselect(column, select.from)
  }

  def selectSql(select: Select[_]): String

  def prefixSql(op: String, parameter: Column[_], relation: Relation): String =
    s"($op ${columnSql(parameter, relation)})"

  def infixSql(op: String, parameter1: Column[_], parameter2: Column[_], relation: Relation): String =
    s"(${columnSql(parameter1, relation)} $op ${columnSql(parameter2, relation)})"

  def postfixSql(op: String, parameter: Column[_], relation: Relation): String =
    s"(${columnSql(parameter, relation)} $op)"

  def doubleInfixSql(op1: String, op2: String, parameter1: Column[_], parameter2: Column[_], parameter3: Column[_], relation: Relation): String =
    s"(${columnSql(parameter1, relation)} $op1 ${columnSql(parameter2, relation)} $op2 ${columnSql(parameter3, relation)})"

  def functionSql(op: String, parameters: Seq[Column[_]], relation: Relation): String =
    parameters.map(parameter => columnSql(parameter, relation)).mkString(s"$op(", ", ", ")")

  def windowFunctionSql(partitionByColumns: Seq[Column[_]], orders: Seq[Order], relation: Relation) = {
    val partitionBy =
      if (partitionByColumns.isEmpty) ""
      else s"partition by ${partitionByColumns.map(column => columnSql(column, relation)).mkString(", ")}"

    val orderBy =
      if (orders.isEmpty) ""
      else s"order by ${orderListSql(orders, relation)}"

    (partitionBy + " " + orderBy).trim
  }

  def orderListSql(orders: Seq[Order], relation: Relation) =
    orders.map(order => orderSql(order, relation)).mkString(", ")

  def groupListSql(groups: Seq[Group], relation: Relation) =
    groups.map(group => groupSql(group, relation)).mkString(", ")

  def orderSql(order: Order, relation: Relation) =
    if (order.ascending)
      columnSql(order.column, relation)
    else
      columnSql(order.column, relation) + " desc"

  def groupSql(group: Group, relation: Relation): String = group match {
    case group: ColumnGroup => columnSql(group.column, relation)
    case group: TupleGroup => group.columns.map(column => groupSql(column, relation)).mkString("(", ", ", ")")
    case group: FunctionGroup => group.name + "(" + group.columns.map(column => groupSql(column, relation)).mkString(", ") + ")"
  }

  def literalSql[A](literal: A) =
    "?"

  def constantSql[A](columnType: ColumnType[A], value: A): String = columnType match {
    case BooleanColumnType => value.toString
    case IntColumnType => value.toString
    case LongColumnType => value.toString
    case DoubleColumnType => value.toString
    case BigDecimalColumnType => value.toString
    case StringColumnType => "'" + escapeSqlString(value.toString) + "'"
    case DateTimeColumnType => value.toString
    case optionType: OptionColumnType[_] =>
      val option = value.asInstanceOf[Option[_]]
      if (option.isEmpty) "null" else constantSql(optionType.baseType, option.get)
    case mappedType: MappedColumnType[A, _] => constantSql(mappedType.baseType, mappedType.write(value.asInstanceOf[A]))
  }

  def identifierSql(identifier: String) =
    identifier

  def escapeSqlString(string: String) =
    string.replace("'", "''")

  def caseSql(whens: List[When[_]], `else`: Option[Column[_]], relation: Relation) = {
    val whenSql = whens.map(when => s"when ${columnSql(when.condition, relation)} then ${columnSql(when.result, relation)}").mkString(" ")
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`, relation)} ").getOrElse("")
    s"case $whenSql ${elseSql}end"
  }

  def caseColumnSql(column: Column[_], mappings: List[(Column[_], Column[_])], `else`: Option[Column[_]], relation: Relation) = {
    val whenSql = mappings.map(mapping => s"when ${columnSql(mapping._1, relation)} then ${columnSql(mapping._2, relation)}").mkString(" ")
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`, relation)} ").getOrElse("")
    s"case ${columnSql(column, relation)} $whenSql ${elseSql}end"
  }

  // -------------------------------------------------

  def columnAliasListArgs(columns: Seq[Column[_]]): List[LiteralColumn[_]] =
    columns.toList flatMap columnAliasArgs

  def columnAliasArgs(column: Column[_]): List[LiteralColumn[_]] = column match {
    case column: TableColumn[_] => Nil
    case column: AliasColumn[_] => columnArgs(column.column)
    case column => columnArgs(column)
  }

  def columnArgs(column: Column[_]): List[LiteralColumn[_]] = column match {
    case column: LiteralColumn[_] => List(column)
    case column: ConstantColumn[_] => Nil
    case PrefixFunctionColumn(_, a) => columnArgs(a)
    case InfixFunctionColumn(_, a, b) => columnArgs(a) ++ columnArgs(b)
    case PostfixFunctionColumn(_, a) => columnArgs(a)
    case DoubleInfixFunctionColumn(_, _, a, b, c) => columnArgs(a) ++ columnArgs(b) ++ columnArgs(c)
    case ScalarFunctionColumn(_, parameters) => parameters.toList flatMap columnArgs
    case WindowFunctionColumn(columns, orders) => columns.toList.flatMap(columnArgs) ++ orders.toList.flatMap(order => columnArgs(order.column))
    case SelectColumn(select) => selectArgs(select)
    case column: TableColumn[_] => Nil
    case column: AliasColumn[_] => Nil
    case column: CaseWhenColumn[_] =>
      column.whens.flatMap(when => columnArgs(when.condition) ++ columnArgs(when.result))
    case column: CaseWhenElseColumn[_] =>
      column.whens.flatMap(when => columnArgs(when.condition) ++ columnArgs(when.result)) ++ columnArgs(column.`else`)
    case column: CaseColumnColumn[_, _] =>
      columnArgs(column.column) ++ column.mappings.flatMap(mapping => columnArgs(mapping._1) ++ columnArgs(mapping._2))
    case column: CaseColumnElseColumn[_, _] =>
      columnArgs(column.column) ++ column.mappings.flatMap(mapping => columnArgs(mapping._1) ++ columnArgs(mapping._2)) ++ columnArgs(column.`else`)
  }

  def selectArgs(select: Select[_]): List[LiteralColumn[_]]

  def orderListArgs(order: Seq[Order]): List[LiteralColumn[_]] =
    order.toList flatMap orderArgs

  def orderArgs(order: Order): List[LiteralColumn[_]] =
    columnArgs(order.column)
}

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
import sqlest.ast.operations.ColumnOperations._

trait BaseStatementBuilder {
  def preprocess(operation: Operation): Operation =
    aliasColumnsFromSubselects(operation)

  def aliasColumnsFromSubselects(operation: Operation): Operation = {
    def matchingColumns(column: Column[_], subselectColumn: AliasedColumn[_]) =
      subselectColumn == column || (column match {
        case aliasedColumn: AliasedColumn[_] => subselectColumn.columnAlias == aliasedColumn.columnAlias
        case _ => false
      })

    operation match {
      case select: Select[_, _] =>
        val subselects = findSubselects(select.from)
        select.mapColumns({
          column =>
            val matchingSubselectColumns = for {
              subselect <- subselects
              subselectColumn <- subselect.columns if matchingColumns(column, subselectColumn)
            } yield subselectColumn

            matchingSubselectColumns
              .headOption
              .map { subselectColumn => ReferenceColumn(subselectColumn.columnAlias)(column.columnType) }
              .getOrElse(column)
        }, operation => aliasColumnsFromSubselects(operation).asInstanceOf[Select[_, _ <: Relation]])
      case update: Update => update.mapColumns(identity, operation => aliasColumnsFromSubselects(operation).asInstanceOf[Select[_, _ <: Relation]])
      case insert: Insert => insert.mapColumns(identity, operation => aliasColumnsFromSubselects(operation).asInstanceOf[Select[_, _ <: Relation]])
      case delete: Delete => delete.mapColumns(identity, operation => aliasColumnsFromSubselects(operation).asInstanceOf[Select[_, _ <: Relation]])
      case _ => operation
    }
  }

  def findSubselects(relation: Relation): List[Select[_, _]] = relation match {
    case _: Table => Nil
    case _: TableFunctionApplication[_] => Nil
    case join: Join[_, _] => findSubselects(join.left) ++ findSubselects(join.right)
    case select: Select[_, _] => List(select) ++ findSubselects(select.from)
  }

  def columnAliasListSql(columns: Seq[Column[_]]): String =
    columns.map(column => columnAliasSql(column)).mkString(", ")

  def columnAliasSql(column: Column[_]): String = column match {
    case column: TableColumn[_] =>
      columnSql(column) + " as " +
        identifierSql(column.columnAlias)

    case column: AliasColumn[_] =>
      columnSql(column) + " as " +
        identifierSql(column.columnAlias)

    case column =>
      columnSql(column)
  }

  def columnSql(column: Column[_]): String =
    // findCellExtractorInRelation(column).getOrElse(
    column match {
      case LiteralColumn(literal) => literalSql(literal)
      case column: ConstantColumn[_] => constantSql(column.columnType, column.value)
      case column: PrefixFunctionColumn[_] => prefixSql(column.name, column.parameter)
      case column: InfixFunctionColumn[_] => infixSql(column.name, column.parameter1, column.parameter2)
      case column: PostfixFunctionColumn[_] => postfixSql(column.name, column.parameter)
      case column: DoubleInfixFunctionColumn[_] => doubleInfixSql(column.infix1, column.infix2, column.parameter1, column.parameter2, column.parameter3)
      case SelectColumn(select) => "(" + selectSql(select) + ")"
      case WindowFunctionColumn(partitionByColumns, orders) => windowFunctionSql(partitionByColumns, orders)
      case column: ScalarFunctionColumn[_] => functionSql(column.name, column.parameters)
      case column: TableColumn[_] => identifierSql(column.tableAlias) + "." + identifierSql(column.columnName)
      case column: AliasColumn[_] => columnSql(column.column)
      case column: ReferenceColumn[_] => column.columnAlias
      case column: CaseWhenColumn[_] => caseSql(column.whens, None)
      case column: CaseWhenElseColumn[_] => caseSql(column.whens, Some(column.`else`))
      case column: CaseColumnColumn[_, _] => caseColumnSql(column.column, column.mappings, None)
      case column: CaseColumnElseColumn[_, _] => caseColumnSql(column.column, column.mappings, Some(column.`else`))
    }

  def selectSql(select: Select[_, _ <: Relation]): String

  def prefixSql(op: String, parameter: Column[_]): String =
    s"($op ${columnSql(parameter)})"

  def infixSql(op: String, parameter1: Column[_], parameter2: Column[_]): String =
    s"(${columnSql(parameter1)} $op ${columnSql(parameter2)})"

  def postfixSql(op: String, parameter: Column[_]): String =
    s"${columnSql(parameter)} $op"

  def doubleInfixSql(op1: String, op2: String, parameter1: Column[_], parameter2: Column[_], parameter3: Column[_]): String =
    s"(${columnSql(parameter1)} $op1 ${columnSql(parameter2)} $op2 ${columnSql(parameter3)})"

  def functionSql(op: String, parameters: Seq[Column[_]]): String =
    parameters.map(parameter => columnSql(parameter)).mkString(s"$op(", ", ", ")")

  def windowFunctionSql(partitionByColumns: Seq[Column[_]], orders: Seq[Order]) = {
    val partitionBy =
      if (partitionByColumns.isEmpty) ""
      else s"partition by ${partitionByColumns.map(column => columnSql(column)).mkString(", ")}"

    val orderBy =
      if (orders.isEmpty) ""
      else s"order by ${orderListSql(orders)}"

    (partitionBy + " " + orderBy).trim
  }

  def orderListSql(orders: Seq[Order]) =
    orders.map(order => orderSql(order)).mkString(", ")

  def groupListSql(groups: Seq[Group]) =
    groups.map(group => groupSql(group)).mkString(", ")

  def orderSql(order: Order) =
    if (order.ascending)
      columnSql(order.column)
    else
      columnSql(order.column) + " desc"

  def groupSql(group: Group): String = group match {
    case group: ColumnGroup => columnSql(group.column)
    case group: TupleGroup => group.groups.map(group => groupSql(group)).mkString("(", ", ", ")")
    case group: FunctionGroup => group.name + "(" + group.groups.map(group => groupSql(group)).mkString(", ") + ")"
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
    case ByteArrayColumnType => javax.xml.bind.DatatypeConverter.printHexBinary(value.asInstanceOf[Array[Byte]])
    case optionType: OptionColumnType[_, _] =>
      val option = value.asInstanceOf[Option[_]]
      if (option.isEmpty) "null" else constantSql(optionType.baseColumnType.asInstanceOf[ColumnType[Any]], option.get)
    case mappedType: MappedColumnType[A, _] => constantSql(mappedType.baseColumnType, mappedType.write(value.asInstanceOf[A]))
  }

  def identifierSql(identifier: String) =
    identifier

  def escapeSqlString(string: String) =
    string.replace("'", "''")

  def caseSql(whens: List[When[_]], `else`: Option[Column[_]]) = {
    val whenSql = whens.map(when => s"when ${columnSql(when.condition)} then ${columnSql(when.result)}").mkString(" ")
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`)} ").getOrElse("")
    s"case $whenSql ${elseSql}end"
  }

  def caseColumnSql(column: Column[_], mappings: List[(Column[_], Column[_])], `else`: Option[Column[_]]) = {
    val whenSql = mappings.map(mapping => s"when ${columnSql(mapping._1)} then ${columnSql(mapping._2)}").mkString(" ")
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`)} ").getOrElse("")
    s"case ${columnSql(column)} $whenSql ${elseSql}end"
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
    case column: AliasColumn[_] => columnArgs(column.column)
    case column: ReferenceColumn[_] => Nil
    case column: CaseWhenColumn[_] =>
      column.whens.flatMap(when => columnArgs(when.condition) ++ columnArgs(when.result))
    case column: CaseWhenElseColumn[_] =>
      column.whens.flatMap(when => columnArgs(when.condition) ++ columnArgs(when.result)) ++ columnArgs(column.`else`)
    case column: CaseColumnColumn[_, _] =>
      columnArgs(column.column) ++ column.mappings.flatMap(mapping => columnArgs(mapping._1) ++ columnArgs(mapping._2))
    case column: CaseColumnElseColumn[_, _] =>
      columnArgs(column.column) ++ column.mappings.flatMap(mapping => columnArgs(mapping._1) ++ columnArgs(mapping._2)) ++ columnArgs(column.`else`)
  }

  def selectArgs(select: Select[_, _ <: Relation]): List[LiteralColumn[_]]
}

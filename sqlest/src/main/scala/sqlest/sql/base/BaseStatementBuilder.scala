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
  protected val MaxWidth = 40
  protected val TabWidth = 4
  protected val NewLine = System.lineSeparator

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
    case tableFunction: TableFunctionFromSelect[_, _] => List(tableFunction.select) ++ findSubselects(tableFunction.select.from)
    case join: Join[_, _] => findSubselects(join.left) ++ findSubselects(join.right)
    case select: Select[_, _] => List(select) ++ findSubselects(select.from)
    case Lateral(select: Select[_, _]) => List(select) ++ findSubselects(select.from)
  }

  def padding(indent: Int): String = " " * indent

  def onNewLine(str: String, indent: Int): String =
    if (str.isEmpty) str
    else NewLine + padding(indent) + str

  def withLineBreaks(strs: Seq[String], indent: Int)(initial: String, separator: String, terminator: String) = {
    if (strs.isEmpty)
      initial + terminator
    else {
      val first = initial + strs.head

      val (sql, _) = strs.tail.foldLeft((first, first.length)) {
        case ((sql, len), next) =>
          val nextLen = next.length
          val newLen = len + separator.length + nextLen
          if (newLen <= MaxWidth)
            (sql + separator + next, newLen)
          else
            (sql + separator + onNewLine(next, indent), indent + nextLen)
      }

      sql + terminator
    }
  }

  def columnAliasListSql(columns: Seq[Column[_]], indent: Int): String = {
    val columnList = columns.map(column => columnAliasSql(column, indent))

    if (columnList.isEmpty)
      ""
    else
      withLineBreaks(columnList, indent)("", ", ", "")
  }

  def columnAliasSql(column: Column[_], indent: Int): String = column match {
    case column: TableColumn[_] =>
      columnSql(column, indent) + " as " +
        identifierSql(column.columnAlias)

    case column: AliasColumn[_] =>
      columnSql(column, indent) + " as " +
        identifierSql(column.columnAlias)

    case column =>
      columnSql(column, indent)
  }

  def columnSql(column: Column[_], indent: Int): String =
    column match {
      case LiteralColumn(literal) => literalSql(literal, indent)
      case column: ConstantColumn[_] => constantSql(column.columnType, column.value)
      case column: PrefixFunctionColumn[_] => prefixSql(column.name, column.parameter, indent)
      case column: InfixFunctionColumn[_] => infixSql(column.name, column.parameter1, column.parameter2, indent)
      case column: PostfixFunctionColumn[_] => postfixSql(column.name, column.parameter, indent)
      case column: DoubleInfixFunctionColumn[_] => doubleInfixSql(column.infix1, column.infix2, column.parameter1, column.parameter2, column.parameter3, indent)
      case SelectColumn(select) => "(" +
        onNewLine(selectSql(select, indent + TabWidth), indent + TabWidth) +
        onNewLine(")", indent)
      case ExistsColumn(select) => "exists (" +
        onNewLine(selectSql(aliasColumnsFromSubselects(select).asInstanceOf[Select[_, _ <: Relation]], indent + TabWidth), indent + TabWidth) +
        onNewLine(")", indent)
      case NotExistsColumn(select) => "not exists (" +
        onNewLine(selectSql(aliasColumnsFromSubselects(select).asInstanceOf[Select[_, _ <: Relation]], indent + TabWidth), indent + TabWidth) +
        onNewLine(")", indent)
      case WindowFunctionColumn(partitionByColumns, orders) =>
        windowFunctionSql(partitionByColumns, orders, indent)
      case column: ScalarFunctionColumn[_] => functionSql(column.name, column.parameters, indent)
      case column: KeywordFunctionColumn[_] => column.name
      case column: TableColumn[_] => identifierSql(column.tableAlias) + "." + identifierSql(column.columnName)
      case column: AliasColumn[_] => columnSql(column.column, indent)
      case column: ReferenceColumn[_] => column.columnAlias
      case column: CaseWhenColumn[_] => caseSql(column.whens, None, indent)
      case column: CaseWhenElseColumn[_] => caseSql(column.whens, Some(column.`else`), indent)
      case column: CaseColumnColumn[_, _] => caseColumnSql(column.column, column.mappings, None, indent)
      case column: CaseColumnElseColumn[_, _] => caseColumnSql(column.column, column.mappings, Some(column.`else`), indent)
    }

  def selectSql(select: Select[_, _ <: Relation], indent: Int): String

  def prefixSql(op: String, parameter: Column[_], indent: Int): String =
    s"($op ${columnSql(parameter, indent)})"

  def infixSql(op: String, parameter1: Column[_], parameter2: Column[_], indent: Int): String =
    if (op.isEmpty)
      s"(${columnSql(parameter1, indent)} ${columnSql(parameter2, indent)})"
    else
      s"(${columnSql(parameter1, indent)} $op ${columnSql(parameter2, indent)})"

  def postfixSql(op: String, parameter: Column[_], indent: Int): String =
    s"${columnSql(parameter, indent)} $op"

  def doubleInfixSql(op1: String, op2: String, parameter1: Column[_], parameter2: Column[_], parameter3: Column[_], indent: Int): String =
    s"(${columnSql(parameter1, indent)} $op1 ${columnSql(parameter2, indent)} $op2 ${columnSql(parameter3, indent)})"

  def functionSql(op: String, parameters: Seq[Column[_]], indent: Int): String = {
    val paramList = parameters.map(parameter => columnSql(parameter, indent))
    withLineBreaks(paramList, indent)(s"$op(", ", ", ")")
  }

  def windowFunctionSql(partitions: Seq[Column[_]], orders: Seq[Order], indent: Int) = {
    val partitionList = partitions.map(column => columnSql(column, indent))

    val partitionBy =
      if (partitionList.isEmpty) ""
      else withLineBreaks(partitionList.tail, indent)(s"partition by ${partitionList.head}", ", ", "")

    val orderList = orders.map(order => orderSql(order, indent))

    val orderBy =
      if (orders.isEmpty) ""
      else withLineBreaks(orderList.tail, indent)(s"order by ${orderList.head}", ", ", "")

    List(partitionBy, orderBy).filterNot(_.isEmpty).mkString(" ")
  }

  def orderListSql(orders: Seq[Order], indent: Int) = {
    val orderList = orders.map(order => orderSql(order, indent))
    if (orders.isEmpty) ""
    else withLineBreaks(orderList, indent)("", ", ", "")
  }

  def groupListSql(groups: Seq[Group], indent: Int) = {
    val groupList = groups.map(group => groupSql(group, indent))
    if (groups.isEmpty) ""
    else withLineBreaks(groupList, indent + TabWidth)("", ", ", "")
  }

  def orderSql(order: Order, indent: Int) =
    if (order.ascending)
      columnSql(order.column, indent)
    else
      columnSql(order.column, indent) + " desc"

  def groupSql(group: Group, indent: Int): String = group match {
    case group: ColumnGroup =>
      columnSql(group.column, indent)
    case group: TupleGroup =>
      val groupList = group.groups.map(group => groupSql(group, indent))
      withLineBreaks(groupList, indent + TabWidth)("(", ", ", ")")
    case group: FunctionGroup =>
      val groupList = group.groups.map(group => groupSql(group, indent))
      withLineBreaks(groupList, indent + TabWidth)(group.name + "(", ", ", ")")
  }

  def literalSql[A](literal: A) = "?"

  def constantSql[A](columnType: ColumnType[A], value: A): String = columnType match {
    case BooleanColumnType => value.toString
    case IntColumnType => value.toString
    case LongColumnType => value.toString
    case DoubleColumnType => value.toString
    case BigDecimalColumnType => value.toString
    case StringColumnType => "'" + escapeSqlString(value.toString) + "'"
    case DateTimeColumnType => value.toString
    case LocalDateColumnType => value.toString
    case ByteArrayColumnType => javax.xml.bind.DatatypeConverter.printHexBinary(value.asInstanceOf[Array[Byte]])
    case optionType: OptionColumnType[_, _] => value.asInstanceOf[Option[_]] match {
      case None if optionType.hasNullNullValue => "null"
      case None => constantSql(optionType.baseColumnType.asInstanceOf[ColumnType[Any]], optionType.nullValue)
      case Some(inner) => constantSql(optionType.innerColumnType.asInstanceOf[ColumnType[Any]], inner)
    }
    case mappedType: MappedColumnType[A, _] => constantSql(mappedType.baseColumnType, mappedType.write(value.asInstanceOf[A]))
  }

  def identifierSql(identifier: String) =
    identifier

  def escapeSqlString(string: String) =
    string.replace("'", "''")

  def caseSql(whens: List[When[_]], `else`: Option[Column[_]], indent: Int) = {
    val whenSql = whens.map(when => s"when ${columnSql(when.condition, indent + TabWidth)} then ${columnSql(when.result, indent)}").mkString(NewLine + padding(indent + TabWidth))
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`, indent + TabWidth)} ").getOrElse("")

    onNewLine(s"case", indent) +
      onNewLine(whenSql, indent + TabWidth) +
      onNewLine(elseSql, indent + TabWidth) +
      onNewLine("end", indent)
  }

  def caseColumnSql(column: Column[_], mappings: List[(Column[_], Column[_])], `else`: Option[Column[_]], indent: Int) = {
    val whenSql = mappings.map(mapping => s"when ${columnSql(mapping._1, indent)} then ${columnSql(mapping._2, indent)}").mkString(NewLine + padding(indent + TabWidth))
    val elseSql = `else`.map(`else` => s"else ${columnSql(`else`, indent)} ").getOrElse("")

    s"case ${columnSql(column, indent)}" +
      onNewLine(whenSql, indent + TabWidth) +
      onNewLine(elseSql, indent + TabWidth) +
      onNewLine("end", indent)
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
    case column: KeywordFunctionColumn[_] => Nil
    case WindowFunctionColumn(columns, orders) => columns.toList.flatMap(columnArgs) ++ orders.toList.flatMap(order => columnArgs(order.column))
    case SelectColumn(select) => selectArgs(select)
    case ExistsColumn(select) => selectArgs(select)
    case NotExistsColumn(select) => selectArgs(select)
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

  def setterArgs[A, B](setter: Setter[A, B]): List[LiteralColumn[_]] = setter match {
    case Setter(tableColumn, column: ConstantColumn[B]) =>
      List(LiteralColumn(column.value.asInstanceOf[A])(tableColumn.columnType))
    case Setter(tableColumn, column) => columnArgs(column).map {
      case LiteralColumn(value) => LiteralColumn(value.asInstanceOf[A])(tableColumn.columnType)
    }
  }
}

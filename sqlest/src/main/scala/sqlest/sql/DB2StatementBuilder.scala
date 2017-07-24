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

package sqlest.sql

import sqlest.ast._
import sqlest.ast.operations.ColumnOperations._

trait DB2StatementBuilder extends base.StatementBuilder {
  override def preprocess(operation: Operation): Operation =
    addTypingToSqlParams(super.preprocess(operation))

  def addTypingToSqlParams(operation: Operation): Operation = operation match {
    case select: Select[_, _] => select.mapColumns(addTypingToSqlFunctions, select => addTypingToSqlParams(select).asInstanceOf[Select[_, _ <: Relation]])
    case update: Update => update.mapColumns(addTypingToSqlFunctions, select => addTypingToSqlParams(select).asInstanceOf[Select[_, _ <: Relation]])
    case insert: Insert => insert.mapColumns(addTypingToSqlFunctions, select => addTypingToSqlParams(select).asInstanceOf[Select[_, _ <: Relation]])
    case delete: Delete => delete.mapColumns(addTypingToSqlFunctions, select => addTypingToSqlParams(select).asInstanceOf[Select[_, _ <: Relation]])
    case _ => operation
  }

  def addTypingToSqlFunctions(column: Column[_]): Column[_] = column match {
    case scalarFunctionColumn: ScalarFunctionColumn[_] => ScalarFunctionColumn(scalarFunctionColumn.name, scalarFunctionColumn.parameters.map(addTypingToSqlColumn))(scalarFunctionColumn.columnType)
    case _ => column
  }

  def addTypingToSqlColumn(column: Column[_]): Column[_] = column match {
    case literalColumn: LiteralColumn[_] => ScalarFunctionColumn("cast", Seq(PostfixFunctionColumn("as " + castLiteralSql(column.columnType), literalColumn)(literalColumn.columnType)))(literalColumn.columnType)
    case _ => column
  }

  def castLiteralSql(columnType: ColumnType[_]): String =
    columnType match {
      case StringColumnType => "varchar(256)"
      case BigDecimalColumnType => "decimal"
      case BooleanColumnType => throw new AssertionError("DB2 does not support Boolean data types")
      case DateTimeColumnType => "timestamp"
      case LocalDateColumnType => "date"
      case DoubleColumnType => "double"
      case IntColumnType => "integer"
      case LongColumnType => "bigint"
      case ByteArrayColumnType => "varbinary(32704)"
      case optionColumnType: OptionColumnType[_, _] => castLiteralSql(optionColumnType.baseColumnType)
      case mappedColumnType: MappedColumnType[_, _] => castLiteralSql(mappedColumnType.baseColumnType)
    }

  override def selectSql(select: Select[_, _ <: Relation], indent: Int): String = {
    val offset = select.offset getOrElse 0L
    if (offset > 0L) {
      rowNumberSelectSql(select, offset, select.limit, indent)
    } else {
      super.selectSql(select, indent)
    }
  }

  override def selectLimitSql(limit: Option[Long]): Option[String] =
    limit map (limit => s"fetch first $limit rows only")

  override def selectOffsetSql(offset: Option[Long]): Option[String] =
    None

  override def joinSql(relation: Relation, indent: Int): String = relation match {
    case tableFunctionApplication: TableFunctionApplication[_] => "table(" + functionSql(tableFunctionApplication.tableName, tableFunctionApplication.parameterColumns.map(addTypingToSqlColumn), indent) + ") as " + identifierSql(tableFunctionApplication.tableAlias)
    case TableFunctionFromSelect(select, alias) =>
      "table(" +
        onNewLine(selectSql(select, indent + TabWidth), indent + TabWidth) +
        onNewLine(") as " + identifierSql(alias), indent)
    case LeftExceptionJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("left exception join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case RightExceptionJoin(left, right, condition) =>
      joinSql(left, indent) +
        onNewLine("right exception join ", indent) +
        joinSql(right, indent) +
        onNewLine("on ", indent) +
        columnSql(condition, indent)
    case _ => super.joinSql(relation, indent)
  }

  def rowNumberSelectSql(select: Select[_, _ <: Relation], offset: Long, limit: Option[Long], indent: Int): String = {
    val orderBy = selectOrderBySql(select.orderBy, indent).getOrElse("")
    val whatColumns = Seq(selectWhatSql(select.columns, indent + TabWidth), s"row_number() over ($orderBy) as rownum")
    val whatSql = withLineBreaks(whatColumns, indent + (TabWidth * 2))("", ", ", "")

    val subquery = Seq(
      whatSql,
      selectFromSql(select.from, indent + TabWidth)
    ) ++ Seq(
        selectWhereSql(select.where, indent + TabWidth),
        selectGroupBySql(select.groupBy, indent + TabWidth)
      ).flatten mkString (NewLine + padding(indent + TabWidth))

    val what =
      withLineBreaks(select.columns.map(col => identifierSql(col.columnAlias)), indent)("", ", ", "")

    val bounds = limit
      .map(limit => s"rownum between ? and ?")
      .getOrElse(s"rownum >= ?")

    s"with subquery as (" +
      onNewLine(subquery, indent + TabWidth) +
      onNewLine(")", indent) +
      onNewLine(s"select $what", indent) +
      onNewLine("from subquery", indent) +
      onNewLine(s"where $bounds", indent)
  }

  override def columnSql(column: Column[_], indent: Int): String =
    column match {
      case literalColumn: LiteralColumn[_] if literalColumn.columnType == BooleanColumnType =>
        if (literalColumn.value == true) "(? = ?)" else "(? <> ?)"
      case constantColumn: ConstantColumn[_] if constantColumn.columnType == BooleanColumnType =>
        if (constantColumn.value == true) "(0 = 0)" else "(0 <> 0)"
      case _ => super.columnSql(column, indent)
    }

  override def selectArgs(select: Select[_, _ <: Relation]): List[LiteralColumn[_]] = {
    val offset = select.offset getOrElse 0L
    if (offset > 0L) {
      rowNumberSelectArgs(select, offset, select.limit)
    } else {
      super.selectArgs(select)
    }
  }

  override def selectLimitArgs(limit: Option[Long]): List[LiteralColumn[_]] =
    Nil

  override def selectOffsetArgs(limit: Option[Long]): List[LiteralColumn[_]] =
    Nil

  def rowNumberSelectArgs(select: Select[_, _ <: Relation], offset: Long, limit: Option[Long]): List[LiteralColumn[_]] = {
    val subqueryArgs =
      selectWhatArgs(select.columns) ++
        selectOrderByArgs(select.orderBy) ++
        selectFromArgs(select.from) ++
        selectWhereArgs(select.where)

    val boundsArgs = limit
      .map(limit => List(LiteralColumn[Long](offset + 1), LiteralColumn[Long](offset + limit)))
      .getOrElse(List(LiteralColumn[Long](offset + 1)))

    subqueryArgs ++ boundsArgs
  }

  override def columnArgs(column: Column[_]): List[LiteralColumn[_]] = column match {
    case column: LiteralColumn[_] if column.columnType == BooleanColumnType => List(LiteralColumn(0), LiteralColumn(0))
    case _ => super.columnArgs(column)
  }

  override def setterArgs[A, B](setter: Setter[A, B]): List[LiteralColumn[_]] = setter match {
    case Setter(tableColumn, column: ConstantColumn[B]) =>
      List(LiteralColumn(column.value.asInstanceOf[A])(tableColumn.columnType))
    case Setter(_, column) if column.columnType == BooleanColumnType =>
      throw new AssertionError("DB2 does not support Boolean data types")
    case _ => super.setterArgs(setter)
  }
}

object DB2StatementBuilder extends DB2StatementBuilder

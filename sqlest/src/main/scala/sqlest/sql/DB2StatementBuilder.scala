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

  override def selectSql(select: Select[_, _ <: Relation]): String = {
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
        selectOffsetSql(select.offset),
        selectLimitSql(select.limit),
        selectOptimizeSql(select.optimize),
        selectUnionSql(select.union)
      ).flatten mkString (" ")
  }

  override def selectLimitSql(limit: Option[Long]): Option[String] =
    limit map (limit => s"fetch first $limit rows only")

  override def selectOffsetSql(offset: Option[Long]): Option[String] =
    offset map (offset => s"offset ${literalSql(offset)} rows")

  override def selectOptimizeSql(optimize: Option[Long]): Option[String] =
    optimize map (optimize => s"optimize for $optimize rows")

  override def joinSql(relation: Relation): String = relation match {
    case tableFunctionApplication: TableFunctionApplication[_] => "table(" + functionSql(tableFunctionApplication.tableName, tableFunctionApplication.parameterColumns.map(addTypingToSqlColumn)) + ") as " + identifierSql(tableFunctionApplication.tableAlias)
    case TableFunctionFromSelect(select, alias) => "table(" + selectSql(select) + ") as " + identifierSql(alias)
    case LeftExceptionJoin(left, right, condition) => joinSql(left) + " left exception join " + joinSql(right) + " on " + columnSql(condition)
    case RightExceptionJoin(left, right, condition) => joinSql(left) + " right exception join " + joinSql(right) + " on " + columnSql(condition)
    case _ => super.joinSql(relation)
  }

  override def columnSql(column: Column[_]): String =
    column match {
      case literalColumn: LiteralColumn[_] if literalColumn.columnType == BooleanColumnType =>
        if (literalColumn.value == true) "(? = ?)" else "(? <> ?)"
      case constantColumn: ConstantColumn[_] if constantColumn.columnType == BooleanColumnType =>
        if (constantColumn.value == true) "(0 = 0)" else "(0 <> 0)"
      case _ => super.columnSql(column)
    }

  override def selectLimitArgs(limit: Option[Long]): List[LiteralColumn[_]] =
    Nil

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

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

package sqlest.ast.operations

import sqlest.ast._

object ColumnOperations {
  implicit class SelectColumnsOps[A, R <: Relation](select: Select[A, R]) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Select[A, _ <: Relation] =
      Select(
        select.aliasedColumns.mapColumns(f, selectFunction, select.cols),
        select.from.mapColumns(f, selectFunction),
        select.where.map(_.mapColumns(f, selectFunction)),
        select.startWith.map(_.mapColumns(f, selectFunction)),
        select.connectBy.map(_.mapColumns(f, selectFunction)),
        select.groupBy.map(_.mapColumns(f, selectFunction)),
        select.having.map(_.mapColumns(f, selectFunction)),
        select.orderBy.map(_.mapColumns(f, selectFunction)),
        select.limit,
        select.offset,
        select.union.map(_.mapColumns(f, selectFunction)),
        select.subselectAlias
      )(select.aliasedColumns)
  }

  implicit class InsertColumnsOps(insert: Insert) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Insert =
      insert match {
        case insertFromSelect: InsertFromSelect[a] =>
          val newSelect = selectFunction(insertFromSelect.select).asInstanceOf[Select[a, _ <: Relation]]
          InsertFromSelect[a](insertFromSelect.into, insertFromSelect.columns.map(_.mapColumns(f, selectFunction).asInstanceOf[TableColumn[_]]), newSelect)(newSelect.aliasedColumns)
        case insertValues: InsertValues =>
          InsertValues(insertValues.into, insertValues.setterLists.map(_.map(_.mapColumns(f, selectFunction))))
      }
  }

  implicit class UpdateColumnsOps(update: Update) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Update =
      Update(update.table, update.set, update.where.map(_.mapColumns(f, selectFunction)))
  }

  implicit class DeleteColumnsOps(delete: Delete) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Delete =
      Delete(delete.from, delete.where.map(_.mapColumns(f, selectFunction)))
  }

  implicit class ColumnOps[A](column: Column[A]) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Column[A] = column match {
      case literalColumn: LiteralColumn[A] => f(literalColumn).asInstanceOf[Column[A]]
      case constantColumn: ConstantColumn[A] => f(constantColumn).asInstanceOf[Column[A]]
      case prefixFunctionColumn: PrefixFunctionColumn[A] => f(PrefixFunctionColumn(prefixFunctionColumn.name, prefixFunctionColumn.parameter.mapColumns(f, selectFunction))(prefixFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case infixFunctionColumn: InfixFunctionColumn[A] => f(InfixFunctionColumn(infixFunctionColumn.name, infixFunctionColumn.parameter1.mapColumns(f, selectFunction), infixFunctionColumn.parameter2.mapColumns(f, selectFunction))(infixFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case postfixFunctionColumn: PostfixFunctionColumn[A] => f(PostfixFunctionColumn(postfixFunctionColumn.name, postfixFunctionColumn.parameter.mapColumns(f, selectFunction))(postfixFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case doubleInfixFunctionColumn: DoubleInfixFunctionColumn[A] => f(DoubleInfixFunctionColumn(doubleInfixFunctionColumn.infix1, doubleInfixFunctionColumn.infix2, doubleInfixFunctionColumn.parameter1.mapColumns(f, selectFunction), doubleInfixFunctionColumn.parameter2.mapColumns(f, selectFunction), doubleInfixFunctionColumn.parameter3.mapColumns(f, selectFunction))(doubleInfixFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case windowFunctionColumn: WindowFunctionColumn => f(WindowFunctionColumn(windowFunctionColumn.partitionByColumns.map(_.mapColumns(f, selectFunction)), windowFunctionColumn.orderBy.map(_.mapColumns(f, selectFunction)))).asInstanceOf[Column[A]]
      case selectColumn: SelectColumn[A] => SelectColumn(selectFunction(selectColumn.select).asInstanceOf[Select[AliasedColumn[A], _ <: Relation]])(selectColumn.columnType).asInstanceOf[Column[A]]
      case scalarFunctionColumn: ScalarFunctionColumn[A] => f(ScalarFunctionColumn(scalarFunctionColumn.name, scalarFunctionColumn.parameters.map(_.mapColumns(f, selectFunction)))(scalarFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case keywordFunctionColumn: KeywordFunctionColumn[A] => f(KeywordFunctionColumn(keywordFunctionColumn.name)(keywordFunctionColumn.columnType)).asInstanceOf[Column[A]]
      case tableColumn: TableColumn[A] => f(tableColumn).asInstanceOf[Column[A]]
      case aliasColumn: AliasColumn[A] => f(AliasColumn(aliasColumn.column.mapColumns(f, selectFunction), aliasColumn.columnAlias)(aliasColumn.columnType)).asInstanceOf[Column[A]]
      case referenceColumn: ReferenceColumn[A] => f(referenceColumn).asInstanceOf[Column[A]]
      case caseWhenColumn: CaseWhenColumn[A] => f(CaseWhenColumn(caseWhenColumn.whens.map(_.mapColumns(f, selectFunction)))(caseWhenColumn.columnType)).asInstanceOf[Column[A]]
      case caseWhenElseColumn: CaseWhenElseColumn[A] => f(CaseWhenElseColumn(caseWhenElseColumn.whens.map(_.mapColumns(f, selectFunction)), caseWhenElseColumn.`else`.mapColumns(f, selectFunction).asInstanceOf[Column[A]])(caseWhenElseColumn.columnType)).asInstanceOf[Column[A]]
      case caseColumnColumn: CaseColumnColumn[_, _] => f(CaseColumnColumn(caseColumnColumn.column.mapColumns(f, selectFunction), caseColumnColumn.mappings.map(mapping => (mapping._1.mapColumns(f, selectFunction), mapping._2.mapColumns(f, selectFunction))))(caseColumnColumn.columnType)).asInstanceOf[Column[A]]
      case caseColumnElseColumn: CaseColumnElseColumn[_, _] => f(CaseColumnElseColumn(caseColumnElseColumn.column.mapColumns(f, selectFunction), caseColumnElseColumn.mappings.map(mapping => (mapping._1.mapColumns(f, selectFunction), mapping._2.mapColumns(f, selectFunction))), caseColumnElseColumn.`else`.mapColumns(f, selectFunction))(caseColumnElseColumn.columnType)).asInstanceOf[Column[A]]
      case existsColumn: ExistsColumn => f(ExistsColumn(selectFunction(existsColumn.select).mapColumns(f, selectFunction))).asInstanceOf[Column[A]]
      case notExistsColumn: NotExistsColumn => f(NotExistsColumn(selectFunction(notExistsColumn.select).mapColumns(f, selectFunction))).asInstanceOf[Column[A]]
    }
  }

  implicit class RelationOps(relation: Relation) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Relation = relation match {
      case table: Table => table
      case TableFunctionApplication(tableName, aliasedAs, parameterColumns, tableFunction) => TableFunctionApplication(tableName, aliasedAs, parameterColumns.map(_.mapColumns(f, selectFunction)), tableFunction)
      case LeftJoin(left, right, condition) => LeftJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case LeftExceptionJoin(left, right, condition) => LeftExceptionJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case RightJoin(left, right, condition) => RightJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case RightExceptionJoin(left, right, condition) => RightExceptionJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case InnerJoin(left, right, condition) => InnerJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case OuterJoin(left, right, condition) => OuterJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction), condition.mapColumns(f, selectFunction))
      case CrossJoin(left, right) => CrossJoin(left.mapColumns(f, selectFunction), right.mapColumns(f, selectFunction))
      case select: Select[_, _] => selectFunction(select)
      case Lateral(select: Select[_, _]) => Lateral(selectFunction(select))
    }
  }

  implicit class OrderOps(order: Order) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Order =
      order.copy(column = order.column.mapColumns(f, selectFunction))
  }

  implicit class GroupOps(group: Group) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Group = group match {
      case ColumnGroup(column) => ColumnGroup(column.mapColumns(f, selectFunction))
      case TupleGroup(groups) => TupleGroup(groups.map(_.mapColumns(f, selectFunction)))
      case FunctionGroup(name, groups) => FunctionGroup(name, groups.map(_.mapColumns(f, selectFunction)))
    }
  }

  implicit class UnionOps[A](union: Union[A]) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Union[A] =
      union.copy(select = union.select.mapColumns(f, selectFunction).asInstanceOf[Select[A, _ <: Relation]])
  }

  implicit class WhenOps[A](when: When[A]) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): When[A] =
      when.copy(condition = when.condition.mapColumns(f, selectFunction), result = when.result.mapColumns(f, selectFunction))
  }

  implicit class SetterOps[A, B](setter: Setter[A, B]) {
    def mapColumns(f: Column[_] => Column[_], selectFunction: Select[_, _ <: Relation] => Select[_, _ <: Relation]): Setter[A, B] =
      new Setter[A, B](setter.column.mapColumns(f, selectFunction).asInstanceOf[TableColumn[A]], setter.value.mapColumns(f, selectFunction).asInstanceOf[Column[B]])
  }
}
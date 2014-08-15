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

package sqlest.ast

import scala.collection.mutable

/**
 * A source of columns from the database.
 * Tables, joins, and select statements are all types of relation.
 */
sealed trait Relation

/**
 * A BaseTable allows columns to be defined on it.
 * It is a base type for Tables and TableFunctions
 */
trait BaseTable {
  def tableName: String
  def aliasedAs: Option[String]
  def tableAlias = aliasedAs getOrElse tableName
  def columns: Seq[TableColumn[_]] = mutableColumns

  /**
   * A mutable view of `columns`, used by the `column`
   * method to append new columns to the relation:
   */
  private val mutableColumns = new mutable.ArrayBuffer[TableColumn[_]]()
  protected def addColumn(column: TableColumn[_]) = mutableColumns += column

  /** Add a column of type `A` to this relation. */
  def column[A](name: String)(implicit columnType: ColumnType[A]) = {
    val ans = TableColumn[A](tableAlias, name)
    this.mutableColumns += ans
    ans
  }

  def column[A](name: String, columnType: MappedColumnType[A, _]) = {
    val ans = TableColumn[A](tableAlias, name)(columnType)
    this.mutableColumns += ans
    ans
  }

  override def toString = {
    if (tableName == tableAlias) {
      s"${getClass.getName}($tableName)"
    } else {
      s"${getClass.getName}($tableName as $tableAlias)"
    }
  }
}

/** A database table. */
abstract class Table(val tableName: String, val aliasedAs: Option[String] = None) extends Relation with BaseTable

/**
 * This TableFunction case class should not be created by the user directly.
 * Instead it will be returned as a result of applying a TableFunctionN (where N is a number)
 * to a set of columns. See TableFunctions for the implementations of TableFunctionN
 */
case class TableFunction(
    tableName: String,
    aliasedAs: Option[String],
    parameterColumns: Seq[Column[_]],
    columns: Seq[TableColumn[_]]) extends Relation {

  def tableAlias = aliasedAs getOrElse tableName
}

/** A join over two relations. */
sealed trait Join extends Relation {
  def left: Relation
  def right: Relation
}

/** A left join between two tables. */
case class LeftJoin(left: Relation, right: Relation, condition: Column[Boolean]) extends Join

/** A right join between two tables. */
case class RightJoin(left: Relation, right: Relation, condition: Column[Boolean]) extends Join

/** An inner join between two tables. */
case class InnerJoin(left: Relation, right: Relation, condition: Column[Boolean]) extends Join

/** An outer join between two tables. */
case class OuterJoin(left: Relation, right: Relation) extends Join

/** A select statement or subselect. */
case class Select[A: AliasedColumns](
    what: A,
    from: Relation,
    where: Option[Column[Boolean]] = None,
    startWith: Option[Column[Boolean]] = None,
    connectBy: Option[Column[Boolean]] = None,
    groupBy: List[Group] = Nil,
    orderBy: List[Order] = Nil,
    limit: Option[Long] = None,
    offset: Option[Long] = None) extends Relation with Query {

  def columns: Seq[AliasedColumn[_]] =
    AliasedColumns[A].toSeq(what)

  def what[A: AliasedColumns](newWhat: A): Select[A] =
    this.copy(what = newWhat)

  def from(relation: Relation): Select[A] =
    this.copy(from = relation)

  def where(expr: Column[Boolean]): Select[A] =
    this.copy(where = this.where map (_ && expr) orElse Some(expr))

  def startWith(expr: Column[Boolean]): Select[A] =
    this.copy(startWith = this.startWith map (_ && expr) orElse Some(expr))

  def connectBy(expr: Column[Boolean]): Select[A] =
    this.copy(connectBy = this.connectBy map (_ && expr) orElse Some(expr))

  def groupBy(groupBys: Group*): Select[A] =
    this.copy(groupBy = this.groupBy ++ groupBys)

  def orderBy(orders: Order*): Select[A] =
    this.copy(orderBy = this.orderBy ++ orders)

  def limit(limit: Long): Select[A] =
    this.copy(limit = Some(limit))

  def offset(offset: Long): Select[A] =
    this.copy(offset = Some(offset))

  def page(number: Long, size: Long): Select[A] =
    this.copy(limit = Some(size), offset = Some(number * size))
}

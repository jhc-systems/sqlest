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

package sqlest.ast.syntax

import scala.annotation.implicitNotFound
import sqlest.ast._

/** DSL syntax for building `Joins`. */
trait JoinSyntax {
  /**
   * Temporary object for building join conditions.
   *
   * Syntax like `a innerJoin b` produces a `SelectJoinBuilder`
   * containing an `on` method to complete the construction of the join.
   */
  trait SelectJoinBuilder[A, R1 <: Relation, R2 <: Relation] {
    def on(condition: Column[Boolean]): Select[A, Join[R1, R2]]
  }

  implicit class SelectJoinOps[A, R1 <: Relation](left: Select[A, R1]) {
    def innerJoin[R2 <: Relation](right: R2) = new SelectJoinBuilder[A, R1, R2] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.innerJoin(right).on(condition))
    }

    def leftJoin[R2 <: Relation](right: R2) = new SelectJoinBuilder[A, R1, R2] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.leftJoin(right).on(condition))
    }

    def rightJoin[R2 <: Relation](right: R2) = new SelectJoinBuilder[A, R1, R2] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.rightJoin(right).on(condition))
    }

    def outerJoin[R2 <: Relation](right: R2) = new SelectJoinBuilder[A, R1, R2] {
      def on(condition: Column[Boolean]): Select[A, Join[R1, R2]] =
        left.from(
          left.from.outerJoin(right).on(condition))
    }

    def crossJoin(right: Relation) = left.from(left.from.crossJoin(right))

    def autoJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.innerJoin(right).on(joinCondition.onClause(left.from, right))

    def autoLeftJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.leftJoin(right).on(joinCondition.onClause(left.from, right))

    def autoRightJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.rightJoin(right).on(joinCondition.onClause(left.from, right))

    def autoOuterJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.outerJoin(right).on(joinCondition.onClause(left.from, right))
  }

  /**
   * Temporary object for building join conditions.
   *
   * Syntax like `a innerJoin b` produces a `JoinBuilder`
   * containing an `on` method to complete the construction of the join.
   */
  trait JoinBuilder[R1 <: Relation, R2 <: Relation] {
    def on(condition: Column[Boolean]): Join[R1, R2]
  }

  /**
   * Typeclass that adds methods such as `leftJoin` and `innerJoin` to a relation.
   */
  implicit class JoinOps[R1 <: Relation](left: R1) {
    def innerJoin[R2 <: Relation](right: R2) = new JoinBuilder[R1, R2] {
      def on(condition: Column[Boolean]) =
        InnerJoin(left, right, condition)
    }

    def leftJoin[R2 <: Relation](right: R2) = new JoinBuilder[R1, R2] {
      def on(condition: Column[Boolean]) =
        LeftJoin(left, right, condition)
    }

    def rightJoin[R2 <: Relation](right: R2) = new JoinBuilder[R1, R2] {
      def on(condition: Column[Boolean]) =
        RightJoin(left, right, condition)
    }

    def outerJoin[R2 <: Relation](right: R2) = new JoinBuilder[R1, R2] {
      def on(condition: Column[Boolean]) =
        OuterJoin(left, right, condition)
    }

    def crossJoin(right: Relation) = new CrossJoin(left, right)

    def autoJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.innerJoin(right).on(joinCondition.onClause(left, right))

    def autoLeftJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.leftJoin(right).on(joinCondition.onClause(left, right))

    def autoRightJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.rightJoin(right).on(joinCondition.onClause(left, right))

    def autoOuterJoin[R2 <: Relation](right: R2)(implicit joinCondition: JoinCondition[R1, R2]) =
      left.outerJoin(right).on(joinCondition.onClause(left, right))
  }

  /**
   * Typeclass witnessing that relations R1 and R2 can be joined
   */
  @implicitNotFound("Either no JoinCondition could be found or ${R2} could be joined to more than 1 table in ${R1}")
  trait JoinCondition[-R1, -R2] {
    def onClause(relation1: R1, relation2: R2): Column[Boolean]
  }

  object JoinCondition {
    def apply[R1, R2](f: (R1, R2) => Column[Boolean]) = new JoinCondition[R1, R2] {
      def onClause(relation1: R1, relation2: R2) = f(relation1, relation2)
    }

    implicit def joinLeftNaturalJoinCondition[R1 <: Relation, R2 <: Relation](implicit joinCondition: JoinCondition[R1, R2]): JoinCondition[Join[R1, _], R2] =
      new JoinCondition[Join[R1, _], R2] {
        def onClause(join: Join[R1, _], relation2: R2) =
          joinCondition.onClause(join.left, relation2)
      }

    implicit def joinRightNaturalJoinCondition[R1 <: Relation, R2 <: Relation](implicit joinCondition: JoinCondition[R1, R2]): JoinCondition[Join[_, R1], R2] =
      new JoinCondition[Join[_, R1], R2] {
        def onClause(join: Join[_, R1], relation2: R2) =
          joinCondition.onClause(join.right, relation2)
      }

    implicit def tableFunctionLeftNaturalJoinCondition[R1, R2 <: Relation](implicit joinCondition: JoinCondition[R1, R2]): JoinCondition[TableFunctionApplication[R1], R2] =
      new JoinCondition[TableFunctionApplication[R1], R2] {
        def onClause(tableFunctionApplication: TableFunctionApplication[R1], relation2: R2) =
          joinCondition.onClause(tableFunctionApplication.tableFunction, relation2)
      }

    implicit def tableFunctionRightNaturalJoinCondition[R1 <: Relation, R2](implicit joinCondition: JoinCondition[R1, R2]): JoinCondition[R1, TableFunctionApplication[R2]] =
      new JoinCondition[R1, TableFunctionApplication[R2]] {
        def onClause(relation1: R1, tableFunctionApplication: TableFunctionApplication[R2]) =
          joinCondition.onClause(relation1, tableFunctionApplication.tableFunction)
      }
  }
}

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

import sqlest.ast._

/** DSL syntax for building `Joins`. */
trait JoinSyntax {
  /**
   * Temporary object for building join conditions.
   *
   * Syntax like `a innerJoin b` produces a `JoinConditionBuilder`
   * containing an `on` method to complete the construction of the join.
   */
  trait SelectJoinConditionBuilder[C] {
    def on(condition: Column[Boolean]): Select[C]
  }

  implicit class SelectJoinOps[C](left: Select[C]) {
    def innerJoin(right: Relation) = new SelectJoinConditionBuilder[C] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.innerJoin(right).on(condition))
    }

    def leftJoin(right: Relation) = new SelectJoinConditionBuilder[C] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.leftJoin(right).on(condition))
    }

    def rightJoin(right: Relation) = new SelectJoinConditionBuilder[C] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.rightJoin(right).on(condition))
    }

    def outerJoin(right: Relation) = new SelectJoinConditionBuilder[C] {
      def on(condition: Column[Boolean]) =
        left.from(left.from.outerJoin(right).on(condition))
    }

    def crossJoin(right: Relation) = left.from(left.from.crossJoin(right))
  }

  /**
   * Temporary object for building join conditions.
   *
   * Syntax like `a innerJoin b` produces a `JoinConditionBuilder`
   * containing an `on` method to complete the construction of the join.
   */
  trait JoinConditionBuilder {
    def on(condition: Column[Boolean]): Relation
  }

  /**
   * Typeclass that adds methods such as `leftJoin` and `innerJoin` to a relation.
   */
  implicit class JoinOps(left: Relation) {
    def innerJoin(right: Relation) = new JoinConditionBuilder {
      def on(condition: Column[Boolean]) =
        InnerJoin(left, right, condition)
    }

    def leftJoin(right: Relation) = new JoinConditionBuilder {
      def on(condition: Column[Boolean]) =
        LeftJoin(left, right, condition)
    }

    def rightJoin(right: Relation) = new JoinConditionBuilder {
      def on(condition: Column[Boolean]) =
        RightJoin(left, right, condition)
    }

    def outerJoin(right: Relation) = new JoinConditionBuilder {
      def on(condition: Column[Boolean]) =
        OuterJoin(left, right, condition)
    }

    def crossJoin(right: Relation) = new CrossJoin(left, right)
  }
}


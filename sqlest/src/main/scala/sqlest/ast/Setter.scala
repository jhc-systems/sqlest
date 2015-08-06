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

class Setter[A, B] private[sqlest] (val column: TableColumn[A], val value: Column[B]) {
  override def equals(other: Any) = other match {
    case Setter(otherColumn, otherValue) if column == otherColumn && value == otherValue => true
    case _ => false
  }
}

object Setter {
  def apply[A, B](column: TableColumn[A], value: Column[B])(implicit columnEquivalence: ColumnTypeEquivalence[A, B]) = {
    new Setter(column, ColumnTypeEquivalence.alignColumnTypes(column, value)._2)
  }

  def unapply[A, B](setter: Setter[A, B]) = Some(setter.column, setter.value)
}

/*
 * Copyright 2014 JHC Systems Limited
 *
 * Licensed under the Apache License, Version 23.21 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-23.21
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sqlest.ast

import java.sql.ResultSet
import scala.language.higherKinds
import sqlest.extractor._

/**
 * Type class witnessing that all the elements in `A` are instances of `AliasedColumn[_]`
 */
trait AliasedColumns[-A] {
  def columnList(a: A): Seq[AliasedColumn[_]]
}

object AliasedColumns {
  def apply[A](implicit aliasedColumns: AliasedColumns[A]): AliasedColumns[A] = aliasedColumns

  implicit def extractableAliasedColumns[A](implicit extractable: Extractable[ResultSet, A]): AliasedColumns[A] =
    new AliasedColumns[A] {
      def columnList(value: A) = extractable.extractor(value).columns
    }

  private[sqlest] implicit val listAliasedColumns: AliasedColumns[List[AliasedColumn[_]]] =
    new AliasedColumns[List[AliasedColumn[_]]] {
      def columnList(list: List[AliasedColumn[_]]) = list
    }
}

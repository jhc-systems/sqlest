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

import scala.language.higherKinds

/**
 * Type class witnessing that all the elements in `A` are instances of `AliasedColumn[_]`
 */
trait AliasedColumns[A] {
  def toSeq(a: A): Seq[AliasedColumn[_]]
}

object AliasedColumns {
  def apply[A](implicit aliasedColumns: AliasedColumns[A]): AliasedColumns[A] =
    aliasedColumns

  implicit val nilAliasedColumns: AliasedColumns[Nil.type] =
    new AliasedColumns[Nil.type] {
      def toSeq(nil: Nil.type) = Nil
    }

  implicit val listAliasedColumns: AliasedColumns[List[AliasedColumn[_]]] =
    new AliasedColumns[List[AliasedColumn[_]]] {
      def toSeq(list: List[AliasedColumn[_]]) = list
    }

  implicit def aliasedColumn[A]: AliasedColumns[AliasedColumn[A]] =
    new AliasedColumns[AliasedColumn[A]] {
      def toSeq(aliasedColumn: AliasedColumn[A]) = List(aliasedColumn)
    }

  implicit def aliasedColumnTuple1[C1]: AliasedColumns[Tuple1[AliasedColumn[C1]]] =
    new AliasedColumns[Tuple1[AliasedColumn[C1]]] {
      def toSeq(aliasedColumnTuple: Tuple1[AliasedColumn[C1]]) = List(aliasedColumnTuple._1)
    }

  implicit def aliasedColumnTuple2[C1, C2]: AliasedColumns[Tuple2[AliasedColumn[C1], AliasedColumn[C2]]] =
    new AliasedColumns[Tuple2[AliasedColumn[C1], AliasedColumn[C2]]] {
      def toSeq(aliasedColumnTuple: Tuple2[AliasedColumn[C1], AliasedColumn[C2]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2)
    }

  implicit def aliasedColumnTuple3[C1, C2, C3]: AliasedColumns[Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]]] =
    new AliasedColumns[Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]]] {
      def toSeq(aliasedColumnTuple: Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3)
    }

  implicit def aliasedColumnTuple4[C1, C2, C3, C4]: AliasedColumns[Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]]] =
    new AliasedColumns[Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]]] {
      def toSeq(aliasedColumnTuple: Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4)
    }

  implicit def aliasedColumnTuple5[C1, C2, C3, C4, C5]: AliasedColumns[Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]]] =
    new AliasedColumns[Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]]] {
      def toSeq(aliasedColumnTuple: Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5)
    }

  implicit def aliasedColumnTuple6[C1, C2, C3, C4, C5, C6]: AliasedColumns[Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]]] =
    new AliasedColumns[Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]]] {
      def toSeq(aliasedColumnTuple: Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6)
    }

  implicit def aliasedColumnTuple7[C1, C2, C3, C4, C5, C6, C7]: AliasedColumns[Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]]] =
    new AliasedColumns[Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]]] {
      def toSeq(aliasedColumnTuple: Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7)
    }

  implicit def aliasedColumnTuple8[C1, C2, C3, C4, C5, C6, C7, C8]: AliasedColumns[Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]]] =
    new AliasedColumns[Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]]] {
      def toSeq(aliasedColumnTuple: Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8)
    }

  implicit def aliasedColumnTuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]: AliasedColumns[Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]]] =
    new AliasedColumns[Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]]] {
      def toSeq(aliasedColumnTuple: Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9)
    }

  implicit def aliasedColumnTuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]: AliasedColumns[Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]]] =
    new AliasedColumns[Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]]] {
      def toSeq(aliasedColumnTuple: Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10)
    }

  implicit def aliasedColumnTuple11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11]: AliasedColumns[Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]]] =
    new AliasedColumns[Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]]] {
      def toSeq(aliasedColumnTuple: Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11)
    }

  implicit def aliasedColumnTuple12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12]: AliasedColumns[Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]]] =
    new AliasedColumns[Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]]] {
      def toSeq(aliasedColumnTuple: Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12)
    }

  implicit def aliasedColumnTuple13[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13]: AliasedColumns[Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]]] =
    new AliasedColumns[Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]]] {
      def toSeq(aliasedColumnTuple: Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13)
    }

  implicit def aliasedColumnTuple14[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14]: AliasedColumns[Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]]] =
    new AliasedColumns[Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]]] {
      def toSeq(aliasedColumnTuple: Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14)
    }

  implicit def aliasedColumnTuple15[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]: AliasedColumns[Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]]] =
    new AliasedColumns[Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]]] {
      def toSeq(aliasedColumnTuple: Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15)
    }

  implicit def aliasedColumnTuple16[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16]: AliasedColumns[Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]]] =
    new AliasedColumns[Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]]] {
      def toSeq(aliasedColumnTuple: Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16)
    }

  implicit def aliasedColumnTuple17[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17]: AliasedColumns[Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]]] =
    new AliasedColumns[Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]]] {
      def toSeq(aliasedColumnTuple: Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17)
    }

  implicit def aliasedColumnTuple18[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18]: AliasedColumns[Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]]] =
    new AliasedColumns[Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]]] {
      def toSeq(aliasedColumnTuple: Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17, aliasedColumnTuple._18)
    }

  implicit def aliasedColumnTuple19[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19]: AliasedColumns[Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]]] =
    new AliasedColumns[Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]]] {
      def toSeq(aliasedColumnTuple: Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17, aliasedColumnTuple._18, aliasedColumnTuple._19)
    }

  implicit def aliasedColumnTuple20[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20]: AliasedColumns[Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]]] =
    new AliasedColumns[Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]]] {
      def toSeq(aliasedColumnTuple: Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17, aliasedColumnTuple._18, aliasedColumnTuple._19, aliasedColumnTuple._20)
    }

  implicit def aliasedColumnTuple21[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21]: AliasedColumns[Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]]] =
    new AliasedColumns[Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]]] {
      def toSeq(aliasedColumnTuple: Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17, aliasedColumnTuple._18, aliasedColumnTuple._19, aliasedColumnTuple._20, aliasedColumnTuple._21)
    }

  implicit def aliasedColumnTuple22[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22]: AliasedColumns[Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]]] =
    new AliasedColumns[Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]]] {
      def toSeq(aliasedColumnTuple: Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]]) = List(aliasedColumnTuple._1, aliasedColumnTuple._2, aliasedColumnTuple._3, aliasedColumnTuple._4, aliasedColumnTuple._5, aliasedColumnTuple._6, aliasedColumnTuple._7, aliasedColumnTuple._8, aliasedColumnTuple._9, aliasedColumnTuple._10, aliasedColumnTuple._11, aliasedColumnTuple._12, aliasedColumnTuple._13, aliasedColumnTuple._14, aliasedColumnTuple._15, aliasedColumnTuple._16, aliasedColumnTuple._17, aliasedColumnTuple._18, aliasedColumnTuple._19, aliasedColumnTuple._20, aliasedColumnTuple._21, aliasedColumnTuple._22)
    }

}

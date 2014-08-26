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

package sqlest.extractor

import scala.annotation.implicitNotFound
import sqlest.ast.AliasedColumn

/**
 * Type class witnessing that a default extractor for type `A` exists
 */
trait Extractable[A] {
  type Out
  def extractor(a: A): SingleExtractor[Out]
}

object Extractable {
  @implicitNotFound("No default extractor for ${A} exists. Pass an extractor to the fetch method")
  type Aux[A, B] = Extractable[A] { type Out = B }

  implicit def extractableColumn[C1]: Extractable.Aux[AliasedColumn[C1], C1] =
    new Extractable[AliasedColumn[C1]] {
      type Out = C1
      def extractor(column: AliasedColumn[C1]) = ColumnExtractor(column)
    }

  implicit def extractableTuple2[C1, C2]: Extractable.Aux[Tuple2[AliasedColumn[C1], AliasedColumn[C2]], Tuple2[C1, C2]] =
    new Extractable[Tuple2[AliasedColumn[C1], AliasedColumn[C2]]] {
      type Out = Tuple2[C1, C2]
      def extractor(columnTuple: Tuple2[AliasedColumn[C1], AliasedColumn[C2]]) = Tuple2Extractor(columnTuple._1, columnTuple._2)
    }

  implicit def extractableTuple3[C1, C2, C3]: Extractable.Aux[Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]], Tuple3[C1, C2, C3]] =
    new Extractable[Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]]] {
      type Out = Tuple3[C1, C2, C3]
      def extractor(columnTuple: Tuple3[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3]]) = Tuple3Extractor(columnTuple._1, columnTuple._2, columnTuple._3)
    }

  implicit def extractableTuple4[C1, C2, C3, C4]: Extractable.Aux[Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]], Tuple4[C1, C2, C3, C4]] =
    new Extractable[Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]]] {
      type Out = Tuple4[C1, C2, C3, C4]
      def extractor(columnTuple: Tuple4[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4]]) = Tuple4Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4)
    }

  implicit def extractableTuple5[C1, C2, C3, C4, C5]: Extractable.Aux[Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]], Tuple5[C1, C2, C3, C4, C5]] =
    new Extractable[Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]]] {
      type Out = Tuple5[C1, C2, C3, C4, C5]
      def extractor(columnTuple: Tuple5[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5]]) = Tuple5Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5)
    }

  implicit def extractableTuple6[C1, C2, C3, C4, C5, C6]: Extractable.Aux[Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]], Tuple6[C1, C2, C3, C4, C5, C6]] =
    new Extractable[Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]]] {
      type Out = Tuple6[C1, C2, C3, C4, C5, C6]
      def extractor(columnTuple: Tuple6[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6]]) = Tuple6Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6)
    }

  implicit def extractableTuple7[C1, C2, C3, C4, C5, C6, C7]: Extractable.Aux[Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]], Tuple7[C1, C2, C3, C4, C5, C6, C7]] =
    new Extractable[Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]]] {
      type Out = Tuple7[C1, C2, C3, C4, C5, C6, C7]
      def extractor(columnTuple: Tuple7[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7]]) = Tuple7Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7)
    }

  implicit def extractableTuple8[C1, C2, C3, C4, C5, C6, C7, C8]: Extractable.Aux[Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]], Tuple8[C1, C2, C3, C4, C5, C6, C7, C8]] =
    new Extractable[Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]]] {
      type Out = Tuple8[C1, C2, C3, C4, C5, C6, C7, C8]
      def extractor(columnTuple: Tuple8[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8]]) = Tuple8Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8)
    }

  implicit def extractableTuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]: Extractable.Aux[Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]], Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] =
    new Extractable[Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]]] {
      type Out = Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]
      def extractor(columnTuple: Tuple9[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9]]) = Tuple9Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9)
    }

  implicit def extractableTuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]: Extractable.Aux[Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]], Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] =
    new Extractable[Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]]] {
      type Out = Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]
      def extractor(columnTuple: Tuple10[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10]]) = Tuple10Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10)
    }

  implicit def extractableTuple11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11]: Extractable.Aux[Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]], Tuple11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11]] =
    new Extractable[Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]]] {
      type Out = Tuple11[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11]
      def extractor(columnTuple: Tuple11[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11]]) = Tuple11Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11)
    }

  implicit def extractableTuple12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12]: Extractable.Aux[Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]], Tuple12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12]] =
    new Extractable[Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]]] {
      type Out = Tuple12[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12]
      def extractor(columnTuple: Tuple12[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12]]) = Tuple12Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12)
    }

  implicit def extractableTuple13[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13]: Extractable.Aux[Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]], Tuple13[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13]] =
    new Extractable[Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]]] {
      type Out = Tuple13[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13]
      def extractor(columnTuple: Tuple13[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13]]) = Tuple13Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13)
    }

  implicit def extractableTuple14[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14]: Extractable.Aux[Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]], Tuple14[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14]] =
    new Extractable[Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]]] {
      type Out = Tuple14[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14]
      def extractor(columnTuple: Tuple14[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14]]) = Tuple14Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14)
    }

  implicit def extractableTuple15[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]: Extractable.Aux[Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]], Tuple15[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]] =
    new Extractable[Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]]] {
      type Out = Tuple15[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]
      def extractor(columnTuple: Tuple15[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15]]) = Tuple15Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15)
    }

  implicit def extractableTuple16[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16]: Extractable.Aux[Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]], Tuple16[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16]] =
    new Extractable[Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]]] {
      type Out = Tuple16[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16]
      def extractor(columnTuple: Tuple16[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16]]) = Tuple16Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16)
    }

  implicit def extractableTuple17[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17]: Extractable.Aux[Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]], Tuple17[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17]] =
    new Extractable[Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]]] {
      type Out = Tuple17[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17]
      def extractor(columnTuple: Tuple17[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17]]) = Tuple17Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17)
    }

  implicit def extractableTuple18[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18]: Extractable.Aux[Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]], Tuple18[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18]] =
    new Extractable[Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]]] {
      type Out = Tuple18[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18]
      def extractor(columnTuple: Tuple18[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18]]) = Tuple18Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17, columnTuple._18)
    }

  implicit def extractableTuple19[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19]: Extractable.Aux[Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]], Tuple19[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19]] =
    new Extractable[Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]]] {
      type Out = Tuple19[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19]
      def extractor(columnTuple: Tuple19[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19]]) = Tuple19Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17, columnTuple._18, columnTuple._19)
    }

  implicit def extractableTuple20[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20]: Extractable.Aux[Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]], Tuple20[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20]] =
    new Extractable[Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]]] {
      type Out = Tuple20[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20]
      def extractor(columnTuple: Tuple20[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20]]) = Tuple20Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17, columnTuple._18, columnTuple._19, columnTuple._20)
    }

  implicit def extractableTuple21[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21]: Extractable.Aux[Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]], Tuple21[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21]] =
    new Extractable[Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]]] {
      type Out = Tuple21[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21]
      def extractor(columnTuple: Tuple21[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21]]) = Tuple21Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17, columnTuple._18, columnTuple._19, columnTuple._20, columnTuple._21)
    }

  implicit def extractableTuple22[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22]: Extractable.Aux[Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]], Tuple22[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22]] =
    new Extractable[Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]]] {
      type Out = Tuple22[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22]
      def extractor(columnTuple: Tuple22[AliasedColumn[C1], AliasedColumn[C2], AliasedColumn[C3], AliasedColumn[C4], AliasedColumn[C5], AliasedColumn[C6], AliasedColumn[C7], AliasedColumn[C8], AliasedColumn[C9], AliasedColumn[C10], AliasedColumn[C11], AliasedColumn[C12], AliasedColumn[C13], AliasedColumn[C14], AliasedColumn[C15], AliasedColumn[C16], AliasedColumn[C17], AliasedColumn[C18], AliasedColumn[C19], AliasedColumn[C20], AliasedColumn[C21], AliasedColumn[C22]]) = Tuple22Extractor(columnTuple._1, columnTuple._2, columnTuple._3, columnTuple._4, columnTuple._5, columnTuple._6, columnTuple._7, columnTuple._8, columnTuple._9, columnTuple._10, columnTuple._11, columnTuple._12, columnTuple._13, columnTuple._14, columnTuple._15, columnTuple._16, columnTuple._17, columnTuple._18, columnTuple._19, columnTuple._20, columnTuple._21, columnTuple._22)
    }
}

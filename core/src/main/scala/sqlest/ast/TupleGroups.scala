/**
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

trait TupleGroups {

  implicit def TupleGroup0(t: Unit) =
    TupleGroup(Nil)

  implicit def TupleGroup1(t: Tuple1[Column[_]]) =
    TupleGroup(List(ColumnGroup(t._1)))

  implicit def TupleGroup2(t: (Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2)))

  implicit def TupleGroup3(t: (Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3)))

  implicit def TupleGroup4(t: (Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4)))

  implicit def TupleGroup5(t: (Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5)))

  implicit def TupleGroup6(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6)))

  implicit def TupleGroup7(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7)))

  implicit def TupleGroup8(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8)))

  implicit def TupleGroup9(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9)))

  implicit def TupleGroup10(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10)))

  implicit def TupleGroup11(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11)))

  implicit def TupleGroup12(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12)))

  implicit def TupleGroup13(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13)))

  implicit def TupleGroup14(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14)))

  implicit def TupleGroup15(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15)))

  implicit def TupleGroup16(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16)))

  implicit def TupleGroup17(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17)))

  implicit def TupleGroup18(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17), ColumnGroup(t._18)))

  implicit def TupleGroup19(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17), ColumnGroup(t._18), ColumnGroup(t._19)))

  implicit def TupleGroup20(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17), ColumnGroup(t._18), ColumnGroup(t._19), ColumnGroup(t._20)))

  implicit def TupleGroup21(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17), ColumnGroup(t._18), ColumnGroup(t._19), ColumnGroup(t._20), ColumnGroup(t._21)))

  implicit def TupleGroup22(t: (Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_], Column[_])) =
    TupleGroup(List(ColumnGroup(t._1), ColumnGroup(t._2), ColumnGroup(t._3), ColumnGroup(t._4), ColumnGroup(t._5), ColumnGroup(t._6), ColumnGroup(t._7), ColumnGroup(t._8), ColumnGroup(t._9), ColumnGroup(t._10), ColumnGroup(t._11), ColumnGroup(t._12), ColumnGroup(t._13), ColumnGroup(t._14), ColumnGroup(t._15), ColumnGroup(t._16), ColumnGroup(t._17), ColumnGroup(t._18), ColumnGroup(t._19), ColumnGroup(t._20), ColumnGroup(t._21), ColumnGroup(t._22)))
}

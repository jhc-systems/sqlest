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

package sqlest.extractor

import java.sql.ResultSet
import sqlest.untyped._

case class Tuple1Extractor[A1](e1: Extractor[A1]) extends ProductExtractor[Tuple1[A1]] {
  type Accumulator = Tuple1[e1.Accumulator]
  val columns = e1.columns.distinct
  val nonOptionalColumns = e1.nonOptionalColumns.distinct

  val innerExtractors = List(e1)

  def initialize(row: ResultSet) =
    Tuple1(e1.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple1(e1.accumulate(row, accumulator._1))

  def emit(accumulator: Accumulator) =
    Tuple1(e1.emit(accumulator._1))

  // def map[B](func: (A1) => B) =
  //   MappedExtractor(this, func.tupled)

  // def as[B](func: (A1) => B)(implicit names: ProductNames[B]) =
  //   sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple2Extractor[A1, A2](e1: Extractor[A1], e2: Extractor[A2]) extends ProductExtractor[Tuple2[A1, A2]] {
  type Accumulator = Tuple2[e1.Accumulator, e2.Accumulator]
  val columns = (e1.columns ++ e2.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2)

  def initialize(row: ResultSet) =
    Tuple2(e1.initialize(row), e2.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple2(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2))

  def emit(accumulator: Accumulator) =
    Tuple2(e1.emit(accumulator._1), e2.emit(accumulator._2))

  def map[B](func: (A1, A2) => B): MappedExtractor[(A1, A2), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple3Extractor[A1, A2, A3](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3]) extends ProductExtractor[Tuple3[A1, A2, A3]] {
  type Accumulator = Tuple3[e1.Accumulator, e2.Accumulator, e3.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3)

  def initialize(row: ResultSet) =
    Tuple3(e1.initialize(row), e2.initialize(row), e3.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple3(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3))

  def emit(accumulator: Accumulator) =
    Tuple3(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3))

  def map[B](func: (A1, A2, A3) => B): MappedExtractor[(A1, A2, A3), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple4Extractor[A1, A2, A3, A4](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4]) extends ProductExtractor[Tuple4[A1, A2, A3, A4]] {
  type Accumulator = Tuple4[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4)

  def initialize(row: ResultSet) =
    Tuple4(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple4(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4))

  def emit(accumulator: Accumulator) =
    Tuple4(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4))

  def map[B](func: (A1, A2, A3, A4) => B): MappedExtractor[(A1, A2, A3, A4), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple5Extractor[A1, A2, A3, A4, A5](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5]) extends ProductExtractor[Tuple5[A1, A2, A3, A4, A5]] {
  type Accumulator = Tuple5[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5)

  def initialize(row: ResultSet) =
    Tuple5(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple5(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5))

  def emit(accumulator: Accumulator) =
    Tuple5(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5))

  def map[B](func: (A1, A2, A3, A4, A5) => B): MappedExtractor[(A1, A2, A3, A4, A5), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple6Extractor[A1, A2, A3, A4, A5, A6](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6]) extends ProductExtractor[Tuple6[A1, A2, A3, A4, A5, A6]] {
  type Accumulator = Tuple6[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6)

  def initialize(row: ResultSet) =
    Tuple6(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple6(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6))

  def emit(accumulator: Accumulator) =
    Tuple6(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6))

  def map[B](func: (A1, A2, A3, A4, A5, A6) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple7Extractor[A1, A2, A3, A4, A5, A6, A7](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7]) extends ProductExtractor[Tuple7[A1, A2, A3, A4, A5, A6, A7]] {
  type Accumulator = Tuple7[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7)

  def initialize(row: ResultSet) =
    Tuple7(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple7(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7))

  def emit(accumulator: Accumulator) =
    Tuple7(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple8Extractor[A1, A2, A3, A4, A5, A6, A7, A8](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8]) extends ProductExtractor[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] {
  type Accumulator = Tuple8[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8)

  def initialize(row: ResultSet) =
    Tuple8(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple8(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8))

  def emit(accumulator: Accumulator) =
    Tuple8(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple9Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9]) extends ProductExtractor[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
  type Accumulator = Tuple9[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9)

  def initialize(row: ResultSet) =
    Tuple9(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple9(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9))

  def emit(accumulator: Accumulator) =
    Tuple9(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple10Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10]) extends ProductExtractor[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] {
  type Accumulator = Tuple10[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)

  def initialize(row: ResultSet) =
    Tuple10(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple10(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10))

  def emit(accumulator: Accumulator) =
    Tuple10(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple11Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11]) extends ProductExtractor[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] {
  type Accumulator = Tuple11[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)

  def initialize(row: ResultSet) =
    Tuple11(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple11(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11))

  def emit(accumulator: Accumulator) =
    Tuple11(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple12Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12]) extends ProductExtractor[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] {
  type Accumulator = Tuple12[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)

  def initialize(row: ResultSet) =
    Tuple12(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple12(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12))

  def emit(accumulator: Accumulator) =
    Tuple12(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple13Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13]) extends ProductExtractor[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] {
  type Accumulator = Tuple13[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)

  def initialize(row: ResultSet) =
    Tuple13(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple13(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13))

  def emit(accumulator: Accumulator) =
    Tuple13(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple14Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14]) extends ProductExtractor[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
  type Accumulator = Tuple14[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)

  def initialize(row: ResultSet) =
    Tuple14(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple14(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14))

  def emit(accumulator: Accumulator) =
    Tuple14(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple15Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15]) extends ProductExtractor[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] {
  type Accumulator = Tuple15[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)

  def initialize(row: ResultSet) =
    Tuple15(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple15(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15))

  def emit(accumulator: Accumulator) =
    Tuple15(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple16Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16]) extends ProductExtractor[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] {
  type Accumulator = Tuple16[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16)

  def initialize(row: ResultSet) =
    Tuple16(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple16(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16))

  def emit(accumulator: Accumulator) =
    Tuple16(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple17Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17]) extends ProductExtractor[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] {
  type Accumulator = Tuple17[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17)

  def initialize(row: ResultSet) =
    Tuple17(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple17(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17))

  def emit(accumulator: Accumulator) =
    Tuple17(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple18Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17], e18: Extractor[A18]) extends ProductExtractor[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] {
  type Accumulator = Tuple18[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator, e18.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns ++ e18.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns ++ e18.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18)

  def initialize(row: ResultSet) =
    Tuple18(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row), e18.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple18(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17), e18.accumulate(row, accumulator._18))

  def emit(accumulator: Accumulator) =
    Tuple18(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17), e18.emit(accumulator._18))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple19Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17], e18: Extractor[A18], e19: Extractor[A19]) extends ProductExtractor[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] {
  type Accumulator = Tuple19[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator, e18.Accumulator, e19.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns ++ e18.columns ++ e19.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns ++ e18.nonOptionalColumns ++ e19.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19)

  def initialize(row: ResultSet) =
    Tuple19(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row), e18.initialize(row), e19.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple19(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17), e18.accumulate(row, accumulator._18), e19.accumulate(row, accumulator._19))

  def emit(accumulator: Accumulator) =
    Tuple19(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17), e18.emit(accumulator._18), e19.emit(accumulator._19))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple20Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17], e18: Extractor[A18], e19: Extractor[A19], e20: Extractor[A20]) extends ProductExtractor[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] {
  type Accumulator = Tuple20[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator, e18.Accumulator, e19.Accumulator, e20.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns ++ e18.columns ++ e19.columns ++ e20.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns ++ e18.nonOptionalColumns ++ e19.nonOptionalColumns ++ e20.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20)

  def initialize(row: ResultSet) =
    Tuple20(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row), e18.initialize(row), e19.initialize(row), e20.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple20(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17), e18.accumulate(row, accumulator._18), e19.accumulate(row, accumulator._19), e20.accumulate(row, accumulator._20))

  def emit(accumulator: Accumulator) =
    Tuple20(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17), e18.emit(accumulator._18), e19.emit(accumulator._19), e20.emit(accumulator._20))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple21Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17], e18: Extractor[A18], e19: Extractor[A19], e20: Extractor[A20], e21: Extractor[A21]) extends ProductExtractor[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] {
  type Accumulator = Tuple21[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator, e18.Accumulator, e19.Accumulator, e20.Accumulator, e21.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns ++ e18.columns ++ e19.columns ++ e20.columns ++ e21.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns ++ e18.nonOptionalColumns ++ e19.nonOptionalColumns ++ e20.nonOptionalColumns ++ e21.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21)

  def initialize(row: ResultSet) =
    Tuple21(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row), e18.initialize(row), e19.initialize(row), e20.initialize(row), e21.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple21(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17), e18.accumulate(row, accumulator._18), e19.accumulate(row, accumulator._19), e20.accumulate(row, accumulator._20), e21.accumulate(row, accumulator._21))

  def emit(accumulator: Accumulator) =
    Tuple21(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17), e18.emit(accumulator._18), e19.emit(accumulator._19), e20.emit(accumulator._20), e21.emit(accumulator._21))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

case class Tuple22Extractor[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](e1: Extractor[A1], e2: Extractor[A2], e3: Extractor[A3], e4: Extractor[A4], e5: Extractor[A5], e6: Extractor[A6], e7: Extractor[A7], e8: Extractor[A8], e9: Extractor[A9], e10: Extractor[A10], e11: Extractor[A11], e12: Extractor[A12], e13: Extractor[A13], e14: Extractor[A14], e15: Extractor[A15], e16: Extractor[A16], e17: Extractor[A17], e18: Extractor[A18], e19: Extractor[A19], e20: Extractor[A20], e21: Extractor[A21], e22: Extractor[A22]) extends ProductExtractor[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] {
  type Accumulator = Tuple22[e1.Accumulator, e2.Accumulator, e3.Accumulator, e4.Accumulator, e5.Accumulator, e6.Accumulator, e7.Accumulator, e8.Accumulator, e9.Accumulator, e10.Accumulator, e11.Accumulator, e12.Accumulator, e13.Accumulator, e14.Accumulator, e15.Accumulator, e16.Accumulator, e17.Accumulator, e18.Accumulator, e19.Accumulator, e20.Accumulator, e21.Accumulator, e22.Accumulator]
  val columns = (e1.columns ++ e2.columns ++ e3.columns ++ e4.columns ++ e5.columns ++ e6.columns ++ e7.columns ++ e8.columns ++ e9.columns ++ e10.columns ++ e11.columns ++ e12.columns ++ e13.columns ++ e14.columns ++ e15.columns ++ e16.columns ++ e17.columns ++ e18.columns ++ e19.columns ++ e20.columns ++ e21.columns ++ e22.columns).distinct
  val nonOptionalColumns = (e1.nonOptionalColumns ++ e2.nonOptionalColumns ++ e3.nonOptionalColumns ++ e4.nonOptionalColumns ++ e5.nonOptionalColumns ++ e6.nonOptionalColumns ++ e7.nonOptionalColumns ++ e8.nonOptionalColumns ++ e9.nonOptionalColumns ++ e10.nonOptionalColumns ++ e11.nonOptionalColumns ++ e12.nonOptionalColumns ++ e13.nonOptionalColumns ++ e14.nonOptionalColumns ++ e15.nonOptionalColumns ++ e16.nonOptionalColumns ++ e17.nonOptionalColumns ++ e18.nonOptionalColumns ++ e19.nonOptionalColumns ++ e20.nonOptionalColumns ++ e21.nonOptionalColumns ++ e22.nonOptionalColumns).distinct

  val innerExtractors = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22)

  def initialize(row: ResultSet) =
    Tuple22(e1.initialize(row), e2.initialize(row), e3.initialize(row), e4.initialize(row), e5.initialize(row), e6.initialize(row), e7.initialize(row), e8.initialize(row), e9.initialize(row), e10.initialize(row), e11.initialize(row), e12.initialize(row), e13.initialize(row), e14.initialize(row), e15.initialize(row), e16.initialize(row), e17.initialize(row), e18.initialize(row), e19.initialize(row), e20.initialize(row), e21.initialize(row), e22.initialize(row))

  def accumulate(row: ResultSet, accumulator: Accumulator) =
    Tuple22(e1.accumulate(row, accumulator._1), e2.accumulate(row, accumulator._2), e3.accumulate(row, accumulator._3), e4.accumulate(row, accumulator._4), e5.accumulate(row, accumulator._5), e6.accumulate(row, accumulator._6), e7.accumulate(row, accumulator._7), e8.accumulate(row, accumulator._8), e9.accumulate(row, accumulator._9), e10.accumulate(row, accumulator._10), e11.accumulate(row, accumulator._11), e12.accumulate(row, accumulator._12), e13.accumulate(row, accumulator._13), e14.accumulate(row, accumulator._14), e15.accumulate(row, accumulator._15), e16.accumulate(row, accumulator._16), e17.accumulate(row, accumulator._17), e18.accumulate(row, accumulator._18), e19.accumulate(row, accumulator._19), e20.accumulate(row, accumulator._20), e21.accumulate(row, accumulator._21), e22.accumulate(row, accumulator._22))

  def emit(accumulator: Accumulator) =
    Tuple22(e1.emit(accumulator._1), e2.emit(accumulator._2), e3.emit(accumulator._3), e4.emit(accumulator._4), e5.emit(accumulator._5), e6.emit(accumulator._6), e7.emit(accumulator._7), e8.emit(accumulator._8), e9.emit(accumulator._9), e10.emit(accumulator._10), e11.emit(accumulator._11), e12.emit(accumulator._12), e13.emit(accumulator._13), e14.emit(accumulator._14), e15.emit(accumulator._15), e16.emit(accumulator._16), e17.emit(accumulator._17), e18.emit(accumulator._18), e19.emit(accumulator._19), e20.emit(accumulator._20), e21.emit(accumulator._21), e22.emit(accumulator._22))

  def map[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => B): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), B] =
    MappedExtractor(this, func.tupled)

  def as[B](func: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => B)(implicit names: ProductNames[B]): MappedExtractor[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), B] =
    sqlest.untyped.extractor.NamedExtractor(this, func.tupled, names.names)
}

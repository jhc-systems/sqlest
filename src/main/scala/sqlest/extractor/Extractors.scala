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

import scala.language.implicitConversions
import sqlest.ast._

trait Extractors {
  implicit def extract[A](column: AliasedColumn[A]): ColumnExtractor[A] =
    ColumnExtractor(column)

  def extract[A1, A2](a1: A1, a2: A2)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2]) =
    new Tuple2Extractor[b1.Result, b2.Result](b1(a1), b2(a2))

  def extract[A1, A2, A3](a1: A1, a2: A2, a3: A3)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3]) =
    new Tuple3Extractor[b1.Result, b2.Result, b3.Result](b1(a1), b2(a2), b3(a3))

  def extract[A1, A2, A3, A4](a1: A1, a2: A2, a3: A3, a4: A4)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4]) =
    new Tuple4Extractor[b1.Result, b2.Result, b3.Result, b4.Result](b1(a1), b2(a2), b3(a3), b4(a4))

  def extract[A1, A2, A3, A4, A5](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5]) =
    new Tuple5Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5))

  def extract[A1, A2, A3, A4, A5, A6](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6]) =
    new Tuple6Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6))

  def extract[A1, A2, A3, A4, A5, A6, A7](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7]) =
    new Tuple7Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8]) =
    new Tuple8Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9]) =
    new Tuple9Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10]) =
    new Tuple10Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11]) =
    new Tuple11Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12]) =
    new Tuple12Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13]) =
    new Tuple13Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14]) =
    new Tuple14Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15]) =
    new Tuple15Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16]) =
    new Tuple16Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17]) =
    new Tuple17Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17], b18: ExtractorBuilder[A18]) =
    new Tuple18Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result, b18.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17), b18(a18))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17], b18: ExtractorBuilder[A18], b19: ExtractorBuilder[A19]) =
    new Tuple19Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result, b18.Result, b19.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17), b18(a18), b19(a19))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17], b18: ExtractorBuilder[A18], b19: ExtractorBuilder[A19], b20: ExtractorBuilder[A20]) =
    new Tuple20Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result, b18.Result, b19.Result, b20.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17), b18(a18), b19(a19), b20(a20))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17], b18: ExtractorBuilder[A18], b19: ExtractorBuilder[A19], b20: ExtractorBuilder[A20], b21: ExtractorBuilder[A21]) =
    new Tuple21Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result, b18.Result, b19.Result, b20.Result, b21.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17), b18(a18), b19(a19), b20(a20), b21(a21))

  def extract[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21, a22: A22)(implicit b1: ExtractorBuilder[A1], b2: ExtractorBuilder[A2], b3: ExtractorBuilder[A3], b4: ExtractorBuilder[A4], b5: ExtractorBuilder[A5], b6: ExtractorBuilder[A6], b7: ExtractorBuilder[A7], b8: ExtractorBuilder[A8], b9: ExtractorBuilder[A9], b10: ExtractorBuilder[A10], b11: ExtractorBuilder[A11], b12: ExtractorBuilder[A12], b13: ExtractorBuilder[A13], b14: ExtractorBuilder[A14], b15: ExtractorBuilder[A15], b16: ExtractorBuilder[A16], b17: ExtractorBuilder[A17], b18: ExtractorBuilder[A18], b19: ExtractorBuilder[A19], b20: ExtractorBuilder[A20], b21: ExtractorBuilder[A21], b22: ExtractorBuilder[A22]) =
    new Tuple22Extractor[b1.Result, b2.Result, b3.Result, b4.Result, b5.Result, b6.Result, b7.Result, b8.Result, b9.Result, b10.Result, b11.Result, b12.Result, b13.Result, b14.Result, b15.Result, b16.Result, b17.Result, b18.Result, b19.Result, b20.Result, b21.Result, b22.Result](b1(a1), b2(a2), b3(a3), b4(a4), b5(a5), b6(a6), b7(a7), b8(a8), b9(a9), b10(a10), b11(a11), b12(a12), b13(a13), b14(a14), b15(a15), b16(a16), b17(a17), b18(a18), b19(a19), b20(a20), b21(a21), b22(a22))
}

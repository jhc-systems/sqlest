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

trait SelectSyntax {
  /** Select all columns from a relation: `select.from(...)`. */
  def from(from: Relation) = Select(Nil, from = from)

  /** Select a subset of columns from a relation: `select(...).from(...)`. */
  def apply() = SelectBuilder(Nil)

  def apply[C1](column1: AliasedColumn[C1]) = SelectBuilder(column1)

  def apply[C1, C2](column1: AliasedColumn[C1], column2: AliasedColumn[C2]) = SelectBuilder((column1, column2))

  def apply[C1, C2, C3](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3]) = SelectBuilder((column1, column2, column3))

  def apply[C1, C2, C3, C4](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4]) = SelectBuilder((column1, column2, column3, column4))

  def apply[C1, C2, C3, C4, C5](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5]) = SelectBuilder((column1, column2, column3, column4, column5))

  def apply[C1, C2, C3, C4, C5, C6](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6]) = SelectBuilder((column1, column2, column3, column4, column5, column6))

  def apply[C1, C2, C3, C4, C5, C6, C7](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17], column18: AliasedColumn[C18]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17], column18: AliasedColumn[C18], column19: AliasedColumn[C19]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18, column19))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17], column18: AliasedColumn[C18], column19: AliasedColumn[C19], column20: AliasedColumn[C20]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18, column19, column20))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17], column18: AliasedColumn[C18], column19: AliasedColumn[C19], column20: AliasedColumn[C20], column21: AliasedColumn[C21]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18, column19, column20, column21))

  def apply[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20, C21, C22](column1: AliasedColumn[C1], column2: AliasedColumn[C2], column3: AliasedColumn[C3], column4: AliasedColumn[C4], column5: AliasedColumn[C5], column6: AliasedColumn[C6], column7: AliasedColumn[C7], column8: AliasedColumn[C8], column9: AliasedColumn[C9], column10: AliasedColumn[C10], column11: AliasedColumn[C11], column12: AliasedColumn[C12], column13: AliasedColumn[C13], column14: AliasedColumn[C14], column15: AliasedColumn[C15], column16: AliasedColumn[C16], column17: AliasedColumn[C17], column18: AliasedColumn[C18], column19: AliasedColumn[C19], column20: AliasedColumn[C20], column21: AliasedColumn[C21], column22: AliasedColumn[C22]) = SelectBuilder((column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18, column19, column20, column21, column22))
}

/** Helper class to enable the `select(...).from(...)` syntax. */
case class SelectBuilder[A: AliasedColumns](what: A) {
  def from(from: Relation) = Select(what, from = from)
}

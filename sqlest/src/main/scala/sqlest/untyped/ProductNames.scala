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

package sqlest.untyped

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

case class ProductNames[A](names: List[String])

object ProductNames {
  implicit def apply[A]: ProductNames[A] = macro applyImpl[A]

  def applyImpl[A: c.WeakTypeTag](c: Context): c.Expr[ProductNames[A]] = {
    import c.universe._

    val names = weakTypeOf[A].decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => "\"" + m.name.toString + "\""
    }.toList

    val namesExpr = c.Expr[List[String]](c.parse(names.toString))

    reify { ProductNames[A](namesExpr.splice) }

    // val tree = q"""ProductNames(List(..$names))"""
    // c.Expr[ProductNames[A]](tree)
  }
}

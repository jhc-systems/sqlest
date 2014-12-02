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

package sqlest.untyped.extractor.syntax

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import sqlest.extractor._

case class NamedExtractSyntax(c: Context) extends ExtractorSyntax {
  def extractNamedImpl[A: c.WeakTypeTag] = {
    import c.universe._

    // Step 1. Extract useful information from the type A:
    val typeOfA = weakTypeOf[A]
    val companion = typeOfA.typeSymbol.companion
    val companionType = companion.typeSignature

    // Step 2. Locate the `apply` methods of the case class we're constructing:
    val applyMethods: List[MethodSymbol] = companionType.decl(TermName("apply")) match {
      case method: MethodSymbol => List(method)
      case termSymbol: TermSymbol => termSymbol.alternatives.collect { case method: MethodSymbol => method }
      case _ => Nil
    }
    val validApplyMethods = applyMethods.filter(_.paramLists.length == 1)
    if (validApplyMethods.isEmpty) c.abort(c.enclosingPosition, s"No apply method found for ${companion.name.toString}")

    val liftedApplyMethods = validApplyMethods.map { applyMethod =>
      // Step 3. Extract the parameter list for the `apply` method
      val caseClassParamTerms = applyMethod.paramLists.flatten.map(_.asTerm)
      val caseClassParamTypes = caseClassParamTerms.map(_.typeSignature)
      val caseClassParamNames = caseClassParamTerms.map(_.name)
      val caseClassParamStrings = caseClassParamTerms.map(_.name.toString.trim)

      // Step 4. Build the target code fragment:
      val funcParams = caseClassParamNames.zip(caseClassParamTypes).map {
        case (param, typ) => q"$param: sqlest.extractor.Extractor[$typ]"
      }

      val paramListLength = caseClassParamTerms.length
      val tupleType =
        if (paramListLength == 1) tq"scala.Tuple1[..$caseClassParamTypes]"
        else tq"(..$caseClassParamTypes)"
      val tupleArg = TermName("arg")
      val tupleAccessors = (1 to paramListLength).toList.map(num => Select(Ident(tupleArg), TermName("_" + num)))

      val namedExtractor = tq"sqlest.untyped.extractor.NamedExtractor[$tupleType, $typeOfA]"
      val productExtractor = productExtractorType(caseClassParamTerms.length)

      q"""
        def apply(..$funcParams) =
          new $namedExtractor(
            new $productExtractor(..$caseClassParamNames),
            ($tupleArg: $tupleType) => $companion.$applyMethod(..$tupleAccessors),
            List(..$caseClassParamStrings)
          )
      """
    }

    // import scala.language.dynamics to avoid having to do so at the call site
    c.Expr(q"import scala.language.dynamics; new Dynamic { ..$liftedApplyMethods }")
  }

  def productExtractorType(size: Int) = {
    import c.universe._
    size match {
      case 1 => tq"sqlest.extractor.Tuple1Extractor"
      case 2 => tq"sqlest.extractor.Tuple2Extractor"
      case 3 => tq"sqlest.extractor.Tuple3Extractor"
      case 4 => tq"sqlest.extractor.Tuple4Extractor"
      case 5 => tq"sqlest.extractor.Tuple5Extractor"
      case 6 => tq"sqlest.extractor.Tuple6Extractor"
      case 7 => tq"sqlest.extractor.Tuple7Extractor"
      case 8 => tq"sqlest.extractor.Tuple8Extractor"
      case 9 => tq"sqlest.extractor.Tuple9Extractor"
      case 10 => tq"sqlest.extractor.Tuple10Extractor"
      case 11 => tq"sqlest.extractor.Tuple11Extractor"
      case 12 => tq"sqlest.extractor.Tuple12Extractor"
      case 13 => tq"sqlest.extractor.Tuple13Extractor"
      case 14 => tq"sqlest.extractor.Tuple14Extractor"
      case 15 => tq"sqlest.extractor.Tuple15Extractor"
      case 16 => tq"sqlest.extractor.Tuple16Extractor"
      case 17 => tq"sqlest.extractor.Tuple17Extractor"
      case 18 => tq"sqlest.extractor.Tuple18Extractor"
      case 19 => tq"sqlest.extractor.Tuple19Extractor"
      case 20 => tq"sqlest.extractor.Tuple20Extractor"
      case 21 => tq"sqlest.extractor.Tuple21Extractor"
      case 22 => tq"sqlest.extractor.Tuple22Extractor"
      case _ => c.abort(c.enclosingPosition, s"Must have 1 to 22 arguments")
    }
  }
}

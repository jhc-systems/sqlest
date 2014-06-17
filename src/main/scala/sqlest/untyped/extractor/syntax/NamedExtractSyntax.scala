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
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros._
import sqlest.ast._
import sqlest.extractor._

object NamedExtractSyntax extends ExtractorSyntax {
  def extractNamedImpl[A: c.WeakTypeTag](c: Context)(extractors: c.Expr[(String, Extractor[_])]*): c.Expr[SingleExtractor[A]] = {
    import c.universe._
    import c.universe.Flag._

    // Step 1. Extract useful information from the ASTs passed to `extractors`:

    // argumentNames  is a List[String]
    // extractorTrees is a List[Tree]
    val (argumentNames, extractorTrees) = extractors.map(_.tree).map {

      case Apply(
        TypeApply(
          Select(
            Apply(
              _, // this is an implicit conversion to `any2ArrowAssoc`
              List(Literal(Constant(argumentName: String)))
              ),
            _ // this is the `->` method name
            ),
          _
          ),
        List(extractorTree)
        ) => (argumentName, extractorTree)

      case other =>
        c.abort(c.enclosingPosition, s"Argument is not of the form string -> extractor: $other")

    }.unzip

    // Step 2. Extract useful information from the types passed to `extractors`:

    // extractorTypes is List(typeOf[Extractor[A]], typeOf[Extractor[B]], ...)
    // argumentTypes  is List(typeOf[A], typeOf[B], ...)
    val (extractorTypes, argumentTypes) = extractors.map(_.actualType).map {

      // The type of each argument is Tuple2[String, Extractor[A]]:
      case TypeRef(_, _, _ :: extractorType :: Nil) =>

        // The extractor can be any subtype of `Extractor` with any number of type args.
        // We cast it to `Extractor[A]` to get its result type as the first type arg:
        extractorType.baseType(typeOf[Extractor[_]].typeSymbol) match {
          case TypeRef(_, _, argumentType :: Nil) =>
            (extractorType, argumentType)
        }

    }.unzip

    // Step 3. Locate the `apply` method of the case class we're constructing:

    val caseClassType = weakTypeOf[A].typeSymbol
    val companion = caseClassType.companion
    val companionType = companion.typeSignature

    // Extractor to match apply methods with the correct number and type of arguments:
    object MatchingApplyMethod {
      def unapply(sym: Symbol) = sym match {
        case method: MethodSymbol =>
          method.paramLists match {
            case firstParamList :: Nil =>
              val paramListMatches =
                firstParamList.map(_.asTerm.typeSignature).zip(argumentTypes).map {
                  case (paramType, argumentType) => argumentType <:< paramType
                }.foldLeft(true)(_ && _)
              if (paramListMatches) Some(method) else None
            case _ => None
          }
        case _ => None
      }
    }

    // Attempt to find an apply method with the same parameter types as extractorTypes:
    val applyMethod = companionType.decl(TermName("apply")) match {
      case NoSymbol =>
        c.abort(c.enclosingPosition, s"No matching apply method found: ${companion.name.toString}(${argumentTypes.mkString(",")})")
      case sym =>
        sym.asMethod.alternatives.collectFirst {
          case MatchingApplyMethod(apply) => apply
        } getOrElse {
          c.abort(c.enclosingPosition, s"No matching apply method found: ${companion.name.toString}(${argumentTypes.mkString(",")})")
        }
    }

    // Step 4. Assert that the argument names passed to the macro match
    //         the argument names from the apply method:

    applyMethod.paramLists.head.zip(argumentNames).zipWithIndex.foreach {
      case ((arg, name), index) =>
        if (arg.name.toString != name) {
          c.abort(c.enclosingPosition, s"Argument $index to ${companion.name.toString}.apply is named '${arg.name.toString}' not '$name'")
        }
    }

    // Step 5. Build the target code fragment:

    val namedExtractor = tq"sqlest.untyped.extractor.NamedExtractor"
    val productExtractor = productExtractorType(c)(extractors.length)
    val tupleType =
      if (extractors.length == 1) tq"scala.Tuple1[..$argumentTypes]"
      else tq"(..$argumentTypes)"
    val tupleArg = TermName("arg")
    val tupleAccessors = (1 to extractors.length).toList.map(num => Select(Ident(tupleArg), TermName("_" + num)))

    val finalTree = q"""
      new $namedExtractor(
        new $productExtractor(..$extractorTrees),
        ($tupleArg: $tupleType) => $companion.$applyMethod(..$tupleAccessors),
        List(..$argumentNames)
      )
    """

    // println("Argument names   | " + argumentNames.map(showRaw(_)))
    // println("Extractor trees  | " + extractorTrees.map(showRaw(_)))
    // println("Extractor types  | " + extractorTypes)
    // println("Argument types   | " + argumentTypes)
    // println("Companion type   | " + companionType)
    // println("Apply method     | " + applyMethod)
    // println("Tuple type       | " + tupleType)
    // println("Final tree       | " + showRaw(finalTree))

    c.Expr[SingleExtractor[A]](finalTree)
  }

  def productExtractorType(c: Context)(size: Int) = {
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

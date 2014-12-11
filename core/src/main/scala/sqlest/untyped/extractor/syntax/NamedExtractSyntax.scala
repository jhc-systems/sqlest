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
  import c.universe._
  import definitions._
  import internal._

  def extractImpl[A: c.WeakTypeTag] = {

    // Extract useful information from the type A:
    val typeOfA = weakTypeOf[A]
    val companion = typeOfA.typeSymbol.companion
    val companionType = companion.typeSignature

    // Locate the `apply` methods of the case class we're constructing:
    val applyMethods = findApplyMethods(companionType)
    if (applyMethods.isEmpty) c.abort(c.enclosingPosition, s"No apply method found for ${companion.name.toString}")

    // Construct extractor methods for this case class corresponding to each apply method
    val liftedApplyMethods = applyMethods.map(liftApplyMethod(_, typeOfA, companion))

    // import scala.language.dynamics to avoid having to do so at the call site
    c.Expr(q"import scala.language.dynamics; new Dynamic { ..$liftedApplyMethods }")
  }

  def findApplyMethods(companionType: Type): List[MethodSymbol] = {
    val applyMethods = companionType.decl(TermName("apply")) match {
      case method: MethodSymbol => List(method)
      case termSymbol: TermSymbol => termSymbol.alternatives.collect { case method: MethodSymbol => method }
      case _ => Nil
    }
    applyMethods.filter(_.paramLists.length == 1)
  }

  def liftApplyMethod(applyMethod: MethodSymbol, typeOfA: Type, companion: Symbol) = {
    // Extract useful information about the parameter list for the `apply` method
    val caseClassParamNames = extractMappedParams(applyMethod, _.name)
    val caseClassParamTypes = extractMappedParams(applyMethod, _.typeSignature)
    val caseClassParamStrings = extractMappedParams(applyMethod, _.name.toString.trim)
    val caseClassParamDefaultValues = extractDefaultValues(extractMappedParams(applyMethod), companion)

    // Build the apply method
    val applyParams = buildApplyParams(caseClassParamNames, caseClassParamTypes, caseClassParamDefaultValues)

    // Build the tuple definition
    val tupleArg = TermName("arg")
    val tupleType = buildTupleType(applyMethod, caseClassParamTypes)
    val tupleAccessors = buildTupleAccessors(applyMethod, tupleArg)

    // Build the extractor definitions
    val namedExtractor = tq"sqlest.untyped.extractor.NamedExtractor[$tupleType, $typeOfA]"
    val tupleExtractor = productExtractorType(extractMappedParams(applyMethod).length)
    val tupleExtractorParams = buildExtractorParams(applyMethod, caseClassParamNames, caseClassParamTypes)

    q"""
        def apply(..$applyParams) =
          new $namedExtractor(
            new $tupleExtractor(..$tupleExtractorParams),
            ($tupleArg: $tupleType) => $companion.$applyMethod(..$tupleAccessors),
            List(..$caseClassParamStrings)
          )
      """
  }

  def extractMappedParams[B](applyMethod: MethodSymbol, f: TermSymbol => B = identity[TermSymbol](_)): List[B] = {
    applyMethod.paramLists.flatten.map(_.asTerm).map(f)
  }

  def extractDefaultValues(paramTerms: List[TermSymbol], companionType: Symbol): List[Option[Tree]] = {
    paramTerms.zipWithIndex.map {
      case (param, index) =>
        if (param.isParamWithDefault) {
          val getterName = TermName("apply$default$" + (index + 1))
          Some(q"$companionType.$getterName")
        } else None
    }
  }

  def buildApplyParams(paramNames: List[TermName], paramTypes: List[Type], defaultValues: List[Option[Tree]]): List[Tree] = {
    val repeatedParamClass = RepeatedParamClass
    val extractorSymbol = c.mirror.staticClass("sqlest.extractor.Extractor")

    val liftRepeatable: PartialFunction[Type, Type] = {
      case TypeRef(_, `repeatedParamClass`, typ) =>
        typeRef(NoPrefix, `repeatedParamClass`, typeRef(extractorSymbol.owner.asType.toType, extractorSymbol, typ) :: Nil)
    }
    val liftParam: PartialFunction[Type, Type] = {
      case inner: TypeRef =>
        typeRef(extractorSymbol.owner.asType.toType, extractorSymbol, inner :: Nil)
    }

    val liftedParamTypes = paramTypes.map(liftRepeatable orElse liftParam)

    (paramNames, liftedParamTypes, defaultValues).zipped.map {
      case (paramName, typ, defaultValue) =>
        if (defaultValue.isDefined)
          q"val $paramName: $typ = sqlest.extractor.ConstantExtractor(${defaultValue.get})"
        else
          q"val $paramName: $typ"
    }
  }

  def buildTupleType(applyMethod: MethodSymbol, paramTypes: List[Type]): Tree = {
    val repeatedParamClass = RepeatedParamClass
    val seqSymbol = c.mirror.staticClass("scala.collection.Seq")

    val tupleType =
      if (applyMethod.isVarargs)
        paramTypes.init :+
          (paramTypes.last match {
            case TypeRef(_, `repeatedParamClass`, typ) =>
              typeRef(seqSymbol.owner.asType.toType, seqSymbol, typ)
          })
      else paramTypes

    if (extractMappedParams(applyMethod).length == 1) tq"scala.Tuple1[..$tupleType]"
    else tq"(..$tupleType)"
  }

  def buildTupleAccessors(applyMethod: MethodSymbol, tupleArg: TermName): List[Tree] = {
    val length = extractMappedParams(applyMethod).length
    val accessors = (1 until length).toList.map(num => Select(Ident(tupleArg), TermName("_" + num)))
    if (applyMethod.isVarargs)
      accessors :+ Typed(Select(Ident(tupleArg), TermName("_" + length)), Ident(typeNames.WILDCARD_STAR))
    else
      accessors :+ Select(Ident(tupleArg), TermName("_" + length))
  }

  def buildExtractorParams(applyMethod: MethodSymbol, paramNames: List[TermName], paramTypes: List[Type]): List[Tree] = {
    if (applyMethod.isVarargs) {
      val varargsBaseType = paramTypes.last match {
        case TypeRef(_, _, typ :: Nil) => typ
      }
      paramNames.init.map(Ident(_)) :+
        q"sqlest.extractor.SeqExtractor[$varargsBaseType](${paramNames.last}.toSeq)"
    } else paramNames.map(Ident(_))
  }

  def productExtractorType(size: Int) = {
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

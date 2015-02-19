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

import scala.reflect.macros.whitebox.Context

case class CaseClassExtractorMacro(c: Context) {
  import c.universe._
  import definitions._

  /**
   * RepeatedParamClass cannot be used in a match unless we bind it
   *  to a val ([[https://issues.scala-lang.org/browse/SI-8483 SI-8483]]).
   */
  val repeatedParamClass = RepeatedParamClass

  def apply[Row: c.WeakTypeTag, A: c.WeakTypeTag] = {
    // Extract useful information from the type A:
    val typeOfRow = weakTypeOf[Row]
    val typeOfA = weakTypeOf[A]
    val typeArgs = typeOfA.dealias.typeArgs
    val companion = typeOfA.typeSymbol.companion

    // Locate the `apply` methods of the case class we're constructing:
    val applyMethods = findApplyMethods(companion.typeSignature)
    if (applyMethods.isEmpty) c.abort(c.enclosingPosition, s"No apply method found for ${companion.name.toString}")

    // Construct extractor methods for this case class corresponding to each apply method
    val liftedApplyMethods = applyMethods.map(liftApplyMethod(_, typeOfRow, typeOfA, typeArgs, companion))

    // import scala.language.dynamics to avoid having to do so at the call site
    q"""
      import scala.language.dynamics
      import scala.language.experimental.macros
      new Dynamic {
        ..$liftedApplyMethods
        def applyDynamic(method: String)(args: Any*): sqlest.extractor.Extractor[$typeOfRow, $typeOfA] = macro sqlest.extractor.AbortMacro.apply
        def applyDynamicNamed(method: String)(args: (String, Any)*): sqlest.extractor.Extractor[$typeOfRow, $typeOfA] = macro sqlest.extractor.AbortMacro.apply
      }
    """
  }

  def findApplyMethods(companionType: Type): List[MethodSymbol] = {
    val applyMethods = companionType.member(TermName("apply")) match {
      case method: MethodSymbol => List(method)
      case termSymbol: TermSymbol => termSymbol.alternatives.collect { case method: MethodSymbol => method }
      case _ => Nil
    }
    applyMethods.filter(_.paramLists.length == 1)
  }

  def liftApplyMethod(applyMethod: MethodSymbol, typeOfRow: Type, typeOfA: Type, typeArgs: List[Type], companion: Symbol) = {
    // Extract useful information about the parameter list for the `apply` method
    val caseClassParamNames = extractMappedParams(applyMethod, _.name)
    val caseClassParamTypes = extractMappedParams(applyMethod, _.typeSignature)
    val caseClassParamStrings = extractMappedParams(applyMethod, _.name.toString.trim)
    val caseClassParamDefaultValues = extractDefaultValues(extractMappedParams(applyMethod), companion)

    // Substitute provided type parameters into the apply method
    val appliedTypeArgTypes = substituteTypeParams(applyMethod, typeArgs, caseClassParamTypes)

    // Build a matching apply method for the extractor
    val applyParams = buildApplyParams(typeOfRow, caseClassParamNames, appliedTypeArgTypes, caseClassParamDefaultValues)

    // Build the tuple definition
    val tupleArg = TermName("arg")
    val tupleType = buildTupleType(applyMethod, appliedTypeArgTypes)
    val tupleAccessors = buildTupleAccessors(applyMethod, tupleArg)

    // Build the extractor definitions
    val tupleExtractor = productExtractorType(extractMappedParams(applyMethod).length)
    val tupleExtractorParams = buildExtractorParams(applyMethod, typeOfRow, caseClassParamNames, appliedTypeArgTypes)

    q"""
      def apply(..$applyParams) = new sqlest.extractor.MappedExtractor[$typeOfRow, $tupleType, $typeOfA](
        new $tupleExtractor(..$tupleExtractorParams) with sqlest.extractor.ProductExtractorNames {
          val innerExtractorNames = List(..$caseClassParamStrings)
        },
        (arg: $tupleType) => $companion.$applyMethod(..$tupleAccessors)
      )
    """
  }

  def extractMappedParams[B](applyMethod: MethodSymbol, f: TermSymbol => B = identity[TermSymbol](_)): List[B] = {
    applyMethod.paramLists.flatten.map(_.asTerm).map(f)
  }

  def extractDefaultValues(paramTerms: List[TermSymbol], companion: Symbol): List[Option[Tree]] = {
    paramTerms.zipWithIndex.map {
      case (param, index) =>
        if (param.isParamWithDefault) {
          val getterName = TermName("apply$default$" + (index + 1))
          Some(q"$companion.$getterName")
        } else None
    }
  }

  def substituteTypeParams(applyMethod: MethodSymbol, typeArgs: List[Type], paramTypes: List[Type]) = {
    if (typeArgs.nonEmpty)
      for (typ <- paramTypes)
        yield typ.substituteTypes(applyMethod.typeParams, typeArgs)
    else paramTypes
  }

  def buildApplyParams(typeOfRow: Type, paramNames: List[TermName], paramTypes: List[Type], defaultValues: List[Option[Tree]]): List[Tree] = {
    val liftParam: PartialFunction[Type, Tree] = {
      case TypeRef(_, `repeatedParamClass`, typ :: Nil) =>
        tq"$repeatedParamClass[sqlest.extractor.Extractor[$typeOfRow, $typ]]"
      case inner: TypeRef =>
        tq"sqlest.extractor.Extractor[$typeOfRow, $inner]"
    }

    val liftedParamTypes = paramTypes.map(liftParam)

    (paramNames, liftedParamTypes, defaultValues).zipped.map {
      case (paramName, typ, defaultValue) =>
        if (defaultValue.isDefined)
          q"val $paramName: $typ = sqlest.extractor.ConstantExtractor(${defaultValue.get})"
        else
          q"val $paramName: $typ"
    }
  }

  def buildTupleType(applyMethod: MethodSymbol, paramTypes: List[Type]): Tree = {
    val treeTypes = paramTypes.map(typ => tq"$typ")

    val tupleType: List[Tree] =
      if (applyMethod.isVarargs)
        treeTypes.init :+
          (paramTypes.last match {
            case TypeRef(_, `repeatedParamClass`, typ :: Nil) => tq"scala.collection.Seq[$typ]"
          })
      else treeTypes

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

  def buildExtractorParams(applyMethod: MethodSymbol, typeOfRow: Type, paramNames: List[TermName], paramTypes: List[Type]): List[Tree] = {
    val treeNames = paramNames.map(name => q"$name")
    if (applyMethod.isVarargs) {
      val varargsBaseType = paramTypes.last match { case TypeRef(_, _, typ :: Nil) => typ }
      treeNames.init :+ q"sqlest.extractor.SeqExtractor[$typeOfRow, $varargsBaseType](${paramNames.last}.toSeq)"
    } else treeNames
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

object AbortMacro {
  def apply(c: Context)(method: c.Tree)(args: c.Tree*) = c.abort(c.enclosingPosition, "Only the apply method can be called here")
}
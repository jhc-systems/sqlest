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

import scala.util.Try

object ExtractorFinder {
  def apply(extractor: Extractor[_], path: String): Option[Extractor[_]] = {
    path.trim match {
      case "" => apply(extractor, Nil)
      case str => apply(extractor, path.split("\\.").toList)
    }
  }

  def apply(extractor: Extractor[_], path: List[String]): Option[Extractor[_]] =
    extractor match {
      case _: CellExtractor[_] =>
        path match {
          case Nil => Some(extractor)
          case _ => None
        }

      case _: ConstantExtractor[_] =>
        path match {
          case Nil => Some(extractor)
          case _ => None
        }

      case productExtractorNames: ProductExtractor[_] with ProductExtractorNames =>
        path match {
          case head :: tail => findByName(productExtractorNames, head).flatMap(apply(_, tail))
          case _ => None
        }

      case productExtractor: ProductExtractor[_] =>
        path match {
          case StringToInt(index) :: tail => findByIndex(productExtractor, index).flatMap(apply(_, tail))
          case _ => None
        }

      case SeqExtractor(extractors) =>
        path match {
          case StringToInt(index) :: tail => apply(extractors(index), tail)
          case _ => None
        }

      case MappedExtractor(inner, _) => apply(inner, path)
      case OptionExtractor(inner) => apply(inner, path)
      case ListMultiRowExtractor(inner) => apply(inner, path)
      case GroupedMultiRowExtractor(inner, _) => apply(inner, path)
    }

  def findByName(productExtractorNames: ProductExtractor[_] with ProductExtractorNames, name: String): Option[Extractor[_]] =
    for {
      index <- productExtractorNames.innerExtractorNames.zipWithIndex.find(_._1 == name).map(_._2)
      extractor <- findByIndex(productExtractorNames, index)
    } yield extractor

  def findByIndex(productExtractor: ProductExtractor[_], index: Int): Option[Extractor[_]] =
    if (index >= 0 && index < productExtractor.innerExtractors.length) {
      Some(productExtractor.innerExtractors(index))
    } else {
      None
    }

  object StringToInt {
    def unapply(str: String) = Try(str.toInt).toOption
  }
}

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

object CellExtractorFinder {
  def apply(extractor: Extractor[_], path: String): Option[CellExtractor[_]] = {
    path.trim match {
      case "" => apply(extractor, Nil)
      case str => apply(extractor, path.split("\\.").toList)
    }
  }

  def apply(extractor: Extractor[_], path: List[String]): Option[CellExtractor[_]] = {
    extractor match {
      case cellExtractor: CellExtractor[_] =>
        path match {
          case Nil => Some(cellExtractor)
          case _ => None
        }

      case named @ NamedExtractor(product: ProductExtractor[_], _) =>
        path match {
          case name :: tail => findByName(named, product, name).flatMap(apply(_, tail))
          case _ => None
        }

      case product: ProductExtractor[_] =>
        path match {
          case StringToInt(index) :: tail => findByIndex(product, index).flatMap(apply(_, tail))
          case _ => None
        }

      case ConstantExtractor(_) => None

      case SeqExtractor(extractors) =>
        path match {
          case StringToInt(index) :: tail => apply(extractors(index), tail)
          case _ => None
        }

      case MappedExtractor(inner, _) =>
        apply(inner, path)

      case OptionExtractor(inner) =>
        apply(inner, path)

      case ListMultiExtractor(inner) =>
        apply(inner, path)

      case GroupedMultiExtractor(inner, _) =>
        apply(inner, path)
    }
  }

  def findByName(named: NamedExtractor[_, _], product: ProductExtractor[_], name: String): Option[Extractor[_]] =
    for {
      index <- named.names.zipWithIndex.find(_._1 == name).map(_._2)
      extractor <- findByIndex(product, index)
    } yield extractor

  def findByIndex(product: ProductExtractor[_], index: Int): Option[Extractor[_]] =
    if (index >= 0 && index < product.innerExtractors.length) {
      Some(product.innerExtractors(index))
    } else {
      None
    }

  object StringToInt {
    def unapply(str: String) = Try(str.toInt).toOption
  }
}
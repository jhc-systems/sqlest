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

package sqlest.untyped.extractor

import sqlest.ast._
import sqlest.extractor._

class NamedExtractor[A, B](inner: Extractor[A], func: A => B, val names: List[String]) extends MappedExtractor[A, B](inner, func) {
  override def toString = s"NamedExtractor($inner,$func,$names)"
}

object NamedExtractor {
  def apply[A, B](inner: Extractor[A], func: A => B, names: List[String]) =
    new NamedExtractor(inner, func, names)

  def unapply[A, B](in: NamedExtractor[A, B]) =
    Some((in.inner, in.func))
}

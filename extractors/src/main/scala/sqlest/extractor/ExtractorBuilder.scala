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

/**
 * Function capable of building an extractor of type `B` from a value of type `A`.
 *
 * We use this in `Extractors` to allow us to build TupleExtractors from
 * mixed tuples of non-extractors and extractors.
 */
trait ExtractorBuilder[-A, B] {
  def apply(preExtractor: A): Extractor[B]
}

object ExtractorBuilder {
  implicit def identityExtractorBuilder[A] = new ExtractorBuilder[Extractor[A], A] {
    def apply(extractor: Extractor[A]) = extractor
  }
}

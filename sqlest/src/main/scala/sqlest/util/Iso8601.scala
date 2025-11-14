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

package sqlest.util

import java.time._
import java.time.format.DateTimeFormatter
import scala.util.Try

object Iso8601 {
  // We can read two formats: with and without milliseconds:

  val msFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS[VV]")

  val secsFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss[VV]")

  // yyyy-MM-dd
  val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  // We write in seconds format by default:
  val defaultFormat = msFormat

  def unapply(str: String): Option[LocalDateTime] =
    Try(LocalDateTime parse (str, msFormat)).toOption orElse
      Try(LocalDateTime parse (str, secsFormat)).toOption orElse
      Try((LocalDate parse (str, dateFormat)).atStartOfDay).toOption

  def apply(date: LocalDateTime) =
    defaultFormat.format(date atZone ZoneId.of("UTC"))
}

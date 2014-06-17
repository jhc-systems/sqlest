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

package sqlest.untyped.ast

import org.joda.time._
import org.joda.time.format._
import scala.util.Try

object Iso8601 {
  // We can read two formats: with and without milliseconds:

  // yyyy-mm-ddThh:mm:ss.ssssZ
  val msFormat = ISODateTimeFormat.dateTime()

  // yyyy-mm-ddThh:mm:ssZ
  val secsFormat = ISODateTimeFormat.dateTimeNoMillis()

  // yyyy-MM-dd
  val dateFormat = ISODateTimeFormat.date()

  // We write in seconds format by default:
  val defaultFormat = msFormat

  def unapply(str: String): Option[DateTime] =
    Try(msFormat parseDateTime str).toOption orElse
      Try(secsFormat parseDateTime str).toOption orElse
      Try(dateFormat parseDateTime str).toOption

  def apply(date: DateTime) =
    defaultFormat.print(date withZone DateTimeZone.UTC)
}

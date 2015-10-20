package sqlest.ast.syntax

import org.joda.time.{ DateTime, LocalDate }
import scala.util.Try
import sqlest.ast._
import sqlest.util.Iso8601

trait UntypedReads[A] {
  type Read
  def reads(s: String): Option[Read]
}

object UntypedReads {
  type Aux[A, B] = UntypedReads[A] { type Read = B }

  object DefaultInstances {
    implicit val untypedBoolean = new UntypedReads[Boolean] {
      type Read = Boolean
      def reads(s: String) = s.trim.toLowerCase match {
        case "true" => Some(true)
        case "false" => Some(false)
        case _ => None
      }
    }

    implicit val untypedString = new UntypedReads[String] {
      type Read = String
      def reads(s: String) = Some(s)
    }

    implicit val untypedInt = new UntypedReads[Int] {
      type Read = Int
      def reads(s: String) = Try(s.toInt).toOption
    }

    implicit val untypedLong = new UntypedReads[Long] {
      type Read = Long
      def reads(s: String) = Try(s.toLong).toOption
    }

    implicit val untypedDouble = new UntypedReads[Double] {
      type Read = Double
      def reads(s: String) = Try(s.toDouble).toOption
    }

    implicit val untypedBigDecimal = new UntypedReads[BigDecimal] {
      type Read = BigDecimal
      def reads(s: String) = Try(BigDecimal(s)).toOption
    }

    implicit val untypedDateTime = new UntypedReads[DateTime] {
      type Read = DateTime
      def reads(s: String) = Iso8601.unapply(s)
    }

    implicit val untypedLocalDate = new UntypedReads[LocalDate] {
      type Read = LocalDate
      def reads(s: String) = Iso8601.unapply(s).map(new LocalDate(_))
    }

    implicit val untypedByteArray = new UntypedReads[Array[Byte]] {
      type Read = Array[Byte]
      def reads(s: String) = Try(javax.xml.bind.DatatypeConverter.parseHexBinary(s)).toOption
    }

    implicit def untypedOption[A, B](implicit U: UntypedReads.Aux[A, B]) = new UntypedReads[Option[A]] {
      type Read = Option[B]
      def reads(s: String) = U.reads(s).map(Some(_))
    }
  }
}


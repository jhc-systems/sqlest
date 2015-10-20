package sqlest.ast.syntax

import org.joda.time.{ DateTime, LocalDate }
import scala.util.Try
import sqlest.ast._
import sqlest.util.Iso8601

trait UntypedReads[A] {
  def reads(s: String): Option[A]
}

object UntypedReads {
  implicit val untypedBoolean = new UntypedReads[Boolean] {
    def reads(s: String) = s.trim.toLowerCase match {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }

  implicit val untypedString = new UntypedReads[String] {
    def reads(s: String) = Some(s)
  }

  implicit val untypedInt = new UntypedReads[Int] {
    def reads(s: String) = Try(s.toInt).toOption
  }

  implicit val untypedLong = new UntypedReads[Long] {
    def reads(s: String) = Try(s.toLong).toOption
  }

  implicit val untypedDouble = new UntypedReads[Double] {
    def reads(s: String) = Try(s.toDouble).toOption
  }

  implicit val untypedBigDecimal = new UntypedReads[BigDecimal] {
    def reads(s: String) = Try(BigDecimal(s)).toOption
  }

  implicit val untypedDateTime = new UntypedReads[DateTime] {
    def reads(s: String) = Iso8601.unapply(s)
  }

  implicit val untypedLocalDate = new UntypedReads[LocalDate] {
    def reads(s: String) = Iso8601.unapply(s).map(new LocalDate(_))
  }

  implicit val untypedByteArray = new UntypedReads[Array[Byte]] {
    def reads(s: String) = Try(javax.xml.bind.DatatypeConverter.parseHexBinary(s)).toOption
  }

  implicit def untypedOption[A](implicit U: UntypedReads[A]) = new UntypedReads[Option[A]] {
    def reads(s: String) = U.reads(s).map(Some(_))
  }
}
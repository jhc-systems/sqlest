package sqlest.executor

case class SqlestException(
  message: String,
  cause: Throwable
) extends Exception(message, cause)

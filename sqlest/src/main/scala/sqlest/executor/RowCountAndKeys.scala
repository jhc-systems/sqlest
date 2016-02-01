package sqlest.executor

case class RowCountAndKeys[T](
  rowsUpdated: Int,
  keys: List[T]
)
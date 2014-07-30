package sqlest.examples

import sqlest._

class FruitTable(alias: Option[String]) extends Table("fruit", alias) {
  val id = column[Int]("id")
  val name = column[String]("name")
  val juiciness = column[Int]("juiciness")
}
object FruitTable extends FruitTable(None)

class SmoothyTable(alias: Option[String]) extends Table("smoothy", alias) {
  val id = column[Int]("id")
  val description = column[String]("description")
}
object SmoothyTable extends SmoothyTable(None)

class IngredientsTable(alias: Option[String]) extends Table("ingredients", alias) {
  val smoothyId = column[Int]("smoothy_id")
  val fruitId = column[Int]("fruit_id")
}
object IngredientsTable extends IngredientsTable(None)
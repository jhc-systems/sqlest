package sqlest.examples

import sqlest._

case class Fruit(name: String, juiciness: Int)
case class Smoothy(description: String, fruits: List[Fruit])

object ExtractorExamples extends App with DatabaseExample {
  InsertExamples.insertAll

  val fruits =
    select(FruitTable.name, FruitTable.juiciness)
      .from(FruitTable)
      .where(FruitTable.juiciness >= 8)
      .orderBy(FruitTable.juiciness.desc)
      .extractAll(fruitExtractor)

  println(fruits)

  lazy val fruitExtractor = extract[Fruit](
    name = FruitTable.name,
    juiciness = FruitTable.juiciness
  )

}

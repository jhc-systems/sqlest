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

  // extractors can be used in the declaration of other extractors
  lazy val smoothyExtractor = extract[Smoothy](
    description = SmoothyTable.description,
    fruits = fruitExtractor.asList
  ).groupBy(SmoothyTable.id)

  // inner joins can be used as follows
  val smoothies =
    select
      .from(SmoothyTable)
      .innerJoin(IngredientsTable).on(SmoothyTable.id === IngredientsTable.smoothyId)
      .innerJoin(FruitTable).on(IngredientsTable.fruitId === FruitTable.id)
      .where(SmoothyTable.description === "Watermelon & grape smoothie")
      .extractAll(smoothyExtractor)

  println(smoothies)

  def selectAll = {
    select(FruitTable.name, FruitTable.juiciness)
      .from(FruitTable)
      .orderBy(FruitTable.juiciness.desc)
      .extractAll(fruitExtractor)
  }

  println(selectAll)
}

package sqlest.examples

import sqlest._

object InsertExamples extends App with DatabaseExample {
  lazy val fruitTableInsertStatement =
    insert
      .into(FruitTable)
      .columns(FruitTable.id, FruitTable.name, FruitTable.juiciness)
      .values(FruitTable.id -> 1, FruitTable.name -> "Watermelon", FruitTable.juiciness -> 10)
      .values(FruitTable.id -> 2, FruitTable.name -> "Tomato", FruitTable.juiciness -> 9)
      .values(FruitTable.id -> 3, FruitTable.name -> "Grape", FruitTable.juiciness -> 8)
      .values(FruitTable.id -> 4, FruitTable.name -> "Banana", FruitTable.juiciness -> 4)

  lazy val smoothyTableInsertStatement =
    insert
      .into(SmoothyTable)
      .columns(SmoothyTable.id, SmoothyTable.description)
      .values(SmoothyTable.id -> 1, SmoothyTable.description -> "Watermelon & grape smoothie")
      .values(SmoothyTable.id -> 2, SmoothyTable.description -> "Super banana smoothie")
      .values(SmoothyTable.id -> 3, SmoothyTable.description -> "Cranberry & raspberry smoothie")

  case class Ingredient(smoothyId: Int, fruitId: Int)
  lazy val extractor = extract[Ingredient](
    smoothyId = IngredientsTable.smoothyId,
    fruitId = IngredientsTable.fruitId
  )

  // We can get the setters for a case class using the extractor
  val indredients = List(Ingredient(1, 1), Ingredient(1, 3))
  lazy val ingredientsTableInsertStatement =
    insert
      .into(IngredientsTable)
      .columns(IngredientsTable.smoothyId, IngredientsTable.fruitId)
      .values(extractor.settersFor(indredients(0)))
      .values(extractor.settersFor(indredients(1)))

  // This also works for lists of the case class
  val moreIndredients = List(Ingredient(2, 4), Ingredient(2, 3))
  lazy val moreIngredientsInsertStatement =
    insert
      .into(IngredientsTable)
      .columns(IngredientsTable.smoothyId, IngredientsTable.fruitId)
      .values(extractor.settersFor(moreIndredients))

  // Write operations must be run in a transaction
  try {
    fruitTableInsertStatement.execute
  } catch {
    case e: AssertionError => println(e.getMessage)
  }

  database.withTransaction {
    val fruitTableNumberInsert = fruitTableInsertStatement.execute
    println(fruitTableNumberInsert)

    val smoothyTableNumberInsert = smoothyTableInsertStatement.execute
    println(smoothyTableNumberInsert)

    val ingredientsTableNumberInsert = ingredientsTableInsertStatement.execute
    println(ingredientsTableNumberInsert)

    val moreIngredientsNumberInsert = moreIngredientsInsertStatement.execute
    println(moreIngredientsNumberInsert)
  }

  def insertAll = database.withTransaction {
    fruitTableInsertStatement.execute
    smoothyTableInsertStatement.execute
    ingredientsTableInsertStatement.execute
  }

}

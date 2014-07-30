package sqlest.examples

import sqlest._

object InsertExamples extends App with DatabaseExample {
  lazy val insertStatement =
    insert
      .into(FruitTable)
      .columns(FruitTable.id, FruitTable.name, FruitTable.juiciness)
      .values(FruitTable.id -> 1, FruitTable.name -> "Watermelon", FruitTable.juiciness -> 10)
      .values(FruitTable.id -> 1, FruitTable.name -> "Tomato", FruitTable.juiciness -> 9)
      .values(FruitTable.id -> 1, FruitTable.name -> "Grape", FruitTable.juiciness -> 8)

  // Write operations must be run in a transaction
  try {
    insertStatement.execute
  } catch {
    case e: AssertionError => println(e.getMessage)
  }

  database.withTransaction {
    val numberInsert = insertStatement.execute
    println(numberInsert)
  }

  def insertAll = database.withTransaction {
    insertStatement.execute
  }

}

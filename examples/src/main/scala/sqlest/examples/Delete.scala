package sqlest.examples

import sqlest._

object DeleteExamples extends App with DatabaseExample {
  InsertExamples.insertAll

  val deleteStatement =
    delete
      .from(FruitTable)
      .where(FruitTable.name === "Banana")

  database.withTransaction { implicit transaction =>
    // running the delete statement returns the number of lines changed (ie deleted)
    val numberDelete = deleteStatement.execute
    println(numberDelete)
  }
}
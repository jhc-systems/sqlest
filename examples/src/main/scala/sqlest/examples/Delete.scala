package sqlest.examples

import sqlest._

object DeleteExamples extends App with DatabaseExample {
  InsertExamples.insertAll

  val deleteStatement =
    delete
      .from(FruitTable)
      .where(FruitTable.name === "Banana")

  // Write operations must be run in a transaction - the below will throw an exception
  try {
    deleteStatement.execute
  } catch {
    case e: AssertionError => println(e.getMessage)
  }

  // try again to run the delete statement in a transaction
  database.withTransaction {
    // running the delete statement returns the number of lines changed (ie deleted)
    val numberDelete = deleteStatement.execute
    println(numberDelete)
  }
}
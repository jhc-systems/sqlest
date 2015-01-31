package sqlest.examples

import sqlest._

object UpdateExamples extends App with DatabaseExample {
  InsertExamples.insertAll

  val updateStatement =
    update(FruitTable)
      .set(FruitTable.juiciness -> 9)
      .where(FruitTable.name === """Banana""")

  database.withTransaction {
    // running execute on the update statement returns the number of lines changed (ie updated)
    val numberInsert = updateStatement.execute
    println(numberInsert)
  }
}

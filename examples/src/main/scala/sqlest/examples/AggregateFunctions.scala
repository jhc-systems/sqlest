package sqlest.examples

import sqlest._

object AggregateFunctions extends App with DatabaseExample {
  // insert some example data
  InsertExamples.insertAll

  // min can be used to efficiently find the min value in a table - here to find the lowest juiciness value in the table
  val minExample = {
    select(min(FruitTable.juiciness))
      .from(FruitTable)
      .fetchHead
  }

  println(minExample)

  // Similarly, max will efficiently return the max value - this will return the highest juiciness value in the table
  val maxExample = {
    select(max(FruitTable.juiciness))
      .from(FruitTable)
      .fetchHead
  }

  println(maxExample)

  // count can be used to count the rows - this will return the number of fruits in our table
  val countExample = {
    select(count())
      .from(FruitTable)
      .fetchHead
  }

  println(countExample)

  // sum will efficiently sum values - this will return the sum of the juiciness values
  val sumExample = {
    select(sum(FruitTable.juiciness))
      .from(FruitTable)
      .fetchHead
  }

  println(sumExample)

  // avg will return the average value for a column - this will return the average juiciness
  // Note: if your column type is an Int, this will return an Int.
  val avgExample = {
    select(avg(FruitTable.juiciness))
      .from(FruitTable)
      .fetchHead
  }

  println(avgExample)
}
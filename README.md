# sqlest

**sqlest** is a database library for Scala. It allows you to write SQL directly in Scala with type safety guarantees while also providing a simple mechanism of extracting domain specific case classes from the results.

**sqlest-extractors** is a second library used for extracting case classes from table data. It is used within sqlest. Check out the [readme](extractors/README.md)

[![Build Status](https://travis-ci.org/jhc-systems/sqlest.svg?branch=master)](https://travis-ci.org/jhc-systems/sqlest)

## Using sqlest
To use sqlest from an existing project add the following resolvers
```scala
resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  // Only needed if you are using a snapshot version of sqlest
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
```

and the following library dependency
```scala
libraryDependencies ++= Seq(
  "co.uk.jhc" %% "sqlest" % "0.6.0"
)
```

sqlest is available for Scala 2.11

## Examples
### Database Connection
Before doing anything else a connection to a database must be available. This is encapsulated in the Database object which requires a DataSource and a StatementBuilder
```scala
import sqlest._

// Configure a DataSource
val dataSource = {
  val dataSource = new org.h2.jdbcx.JdbcDataSource
  dataSource.setURL("jdbc:h2:~/test")
  dataSource
}

// Choose the StatementBuilder that is compatible with the database you are using
val statementBuilder = sqlest.sql.H2StatementBuilder

// Use the DataSource and the StatementBuilder to create an implicit database
// This database is used in all execute calls
implicit val database = Database.withDataSource(dataSource, statementBuilder)
```

### Table and column definitions
A table definition consists of a table name and any columns that you want to use. A table can be created with an alias but it is standard to create an object that has no alias
```scala
class FruitTable(alias: Option[String]) extends Table("fruit", alias) {
  val id = column[Int]("id")
  val name = column[String]("name")
  val juiciness = column[Int]("juiciness")
}
// Create a FruitTable object with no alias
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
```

### Domain classes
Let's define some domain classes that we want to populate from our database
```scala
case class Fruit(name: String, juiciness: Int)
case class Smoothy(description: String, fruits: List[Fruit])
```

### Querying
Queries are written in sqlest the same way as they are written in SQL. The table and column definitions are used directly. Here is a query that will return the juiciest fruits
```scala
select(FruitTable.name, FruitTable.juiciness)
  .from(FruitTable)
  .where(FruitTable.juiciness >= 8)
  .orderBy(FruitTable.juiciness.desc)
  .fetchAll(fruitExtractor)    // fruitExtractor is defined below

==> List(
      Fruit("Watermelon", 10),
      Fruit("Tomato", 9),
      Fruit("Grape", 8)
    )
```

### Extractors
Extractors are used to populate domain classes from ResultSets returned by running queries. They declaratively specify which parameter in a case class is populated by which column in a table
```scala
lazy val fruitExtractor = extract[Fruit](
  name = FruitTable.name,
  juiciness = FruitTable.juiciness
)
```

Extractors are designed for composition to allow nested case classes to be extracted, and multiple rows of the ResultSet to be combined into a single result
```scala
lazy val smoothyExtractor = extract[Smoothy](
  description = SmoothyTable.description,
  fruits = fruitExtractor.asList
).groupBy(SmoothyTable.id) // All results with the same value for this field are combined into a single result
```

This extractor can then be used to find out which fruits are used in a smoothy
```scala
select
  .from(SmoothyTable)
  .innerJoin(IngredientsTable).on(SmoothyTable.id === IngredientsTable.smoothyId)
  .innerJoin(FruitTable).on(IngredientsTable.fruitId === FruitTable.id)
  .where(SmoothyTable.description === "Magic dream shake")
  .fetchOne(smoothyExtractor)

==> Some(
      Smoothy(
        "Magic dream shake",
        List(
          Fruit("Banana", 2),
          Fruit("Orange", 7),
          Fruit("Watermelon", 10),
          Fruit("Grape", 8)
        )
      )
    )
```

## Authors
- [Dave Gurnell](https://github.com/davegurnell)
- [Brendan Maginnis](https://github.com/brendanator)

## Contributors
- [David Gregory](https://github.com/DavidGregory084)

## Acknowledgements
- [Dean Chapman](https://github.com/p14n) - author of Sqler which inspired sqlest
- [Frank Wallis](https://github.com/frankwallis) - ideas, feedback and contributions
- [Slick](https://github.com/slick/slick) - a great project which contains many ideas used in sqlest
- [jOOQ](https://github.com/jOOQ/jOOQ) - a similar project written in Java which provided many ideas used in sqlest

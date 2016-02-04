# sqlest

**sqlest** is a database library for Scala. It allows you to write SQL directly in Scala with type safety guarantees while also providing a simple mechanism of extracting domain specific case classes from the results.

**sqlest-extractors** is a second library used for extracting case classes from table data. It is used within sqlest. Check out the [readme](extractors/README.md)

[![Build Status](https://travis-ci.org/jhc-systems/sqlest.svg?branch=master)](https://travis-ci.org/jhc-systems/sqlest?branch=master) [![codecov.io](https://codecov.io/github/jhc-systems/sqlest/coverage.svg?branch=master)](https://codecov.io/github/jhc-systems/sqlest?branch=master)

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
  "uk.co.jhc" %% "sqlest" % "0.7.3"
)
```

sqlest is available for Scala 2.11

## Examples
### Database Connection
Before doing anything else a connection to a database must be available. This is encapsulated in the Database object which requires a DataSource and a StatementBuilder.
```tut:silent
import sqlest._
```

Configure a DataSource
```tut:silent
val dataSource = {
  val dataSource = new org.h2.jdbcx.JdbcDataSource
  dataSource.setURL("jdbc:h2:~/test")
  dataSource
}
```

Choose the StatementBuilder that is compatible with the database you are using.
```tut:silent
val statementBuilder = sqlest.sql.H2StatementBuilder
```

Use the DataSource and the StatementBuilder to create an implicit database.
This database is used in all execute calls.
```tut:silent
implicit val database = Database.withDataSource(dataSource, statementBuilder)
```
```tut:invisible
def executeRawSql(sql: String) =
  database.withConnection { connection =>
    try {
      connection.createStatement.execute(sql)
    } catch {
      case e: Exception => println(e)// Ignore exception from dropping tables
    }
  }
executeRawSql("drop table fruit")
executeRawSql("""
  create table fruit (
    id int not null,
    name varchar not null,
    juiciness int not null
  )
""")
executeRawSql("drop table smoothy")
executeRawSql("""
  create table smoothy (
    id int not null,
    description varchar not null
  )
""")
executeRawSql("drop table ingredients")
executeRawSql("""
  create table ingredients (
    smoothy_id int not null,
    fruit_id int not null
  )
""")
executeRawSql("""
  insert into fruit(id, name, juiciness)
  values(1, 'Watermelon', 10),
        (2, 'Tomato', 9),
        (3, 'Grape', 8),
        (4, 'Banana', 4)
""")
executeRawSql("""
  insert into smoothy(id, description)
  values(1, 'Watermelon & grape smoothie'),
        (2, 'Super banana smoothie'),
        (3, 'Cranberry & raspberry smoothie')
""")
executeRawSql("""
  insert into ingredients(smoothy_id, fruit_id)
  values(1, 1),
        (1, 3),
        (2, 4),
        (2, 3)
""")
```
### Table and column definitions
A table definition consists of a table name and any columns that you want to use. A table can be created with an alias but it is standard to create an object that has no alias.
```tut:silent
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
```

### Domain classes
Let's define some domain classes that we want to populate from our database
```tut:silent
case class Fruit(name: String, juiciness: Int)
case class Smoothy(description: String, fruits: List[Fruit])
```

### Extractors
Extractors are used to populate domain classes from ResultSets returned by running queries. They declaratively specify which parameter in a case class is populated by which column in a table
```tut:silent
lazy val fruitExtractor = extract[Fruit](
  name = FruitTable.name,
  juiciness = FruitTable.juiciness
)
```

### Querying
Queries are written in sqlest the same way as they are written in SQL. The table and column definitions are used directly. Here is a query that will return the juiciest fruits
```tut
select(FruitTable.name, FruitTable.juiciness).
  from(FruitTable).
  where(FruitTable.juiciness >= 8).
  orderBy(FruitTable.juiciness.desc).
  extractAll(fruitExtractor)    // fruitExtractor is defined below
```

Extractors are designed for composition to allow nested case classes to be extracted, and multiple rows of the ResultSet to be combined into a single result
```tut:silent
lazy val smoothyExtractor = extract[Smoothy](
  description = SmoothyTable.description,
  fruits = fruitExtractor.asList
).groupBy(SmoothyTable.id) // All results with the same value for this field are combined into a single result
```

This extractor can then be used to find out which fruits are used in a smoothy
```tut
select.
  from(SmoothyTable).
  innerJoin(IngredientsTable).on(SmoothyTable.id === IngredientsTable.smoothyId).
  innerJoin(FruitTable).on(IngredientsTable.fruitId === FruitTable.id).
  where(SmoothyTable.description === "Super banana smoothie").
  extractHead(smoothyExtractor)
```

## Authors
- [Dave Gurnell](https://github.com/davegurnell)
- [Brendan Maginnis](https://github.com/brendanator)

## Contributors
- [David Gregory](https://github.com/DavidGregory084)
- [Hamish Dickson](https://github.com/hamishdickson)
- [Drew Kutchar](https://github.com/kutchar)
- [David Sarginson](https://github.com/ShaolinSarg)

## Acknowledgements
- [Dean Chapman](https://github.com/p14n) - author of Sqler which inspired sqlest
- [Frank Wallis](https://github.com/frankwallis) - ideas, feedback and contributions
- [Slick](https://github.com/slick/slick) - a great project which contains many ideas used in sqlest
- [jOOQ](https://github.com/jOOQ/jOOQ) - a similar project written in Java which provided many ideas used in sqlest

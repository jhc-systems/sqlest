# sqlest-extractors

**sqlest-extractors** is a small library for extracting case classes from table data:

- Minimal work to set up sqlest-extractors with a new table type
- Extractors are defined declaratively and are easily composed
- Support for:
  - Tuples
  - Options
  - List, Seq, Map, etc.
  - Mapped values
  - Grouping rows into single results

## Using sqlest-extractors
To use sqlest-extractors from an existing project add the following resolvers
```scala
resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  // Only needed if you are using a snapshot version of sqlest-extractors
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
```

and the following library dependency
```scala
libraryDependencies ++= Seq(
  "uk.co.jhc" %% "sqlest-extractors" % "0.7.2"
)
```

sqlest-extractors is available for Scala 2.11

## Overview
Table data consists of multiple `Rows` of cells.

To use sqlest-extractors:

1. Determine the `Row` type
2. Extend the `CellExtractor` trait to read a value from a cell in a `Row`
3. Mixin `ExtractorSyntax[Row]`

## Example - CSV
```scala
object CSVDefinitions {
  // Start with some type aliases for CSV data
  type CSVRow = List[String]
  type CSV = List[CSVRow]

  // Now define a simple parser
  def parse(input: String): CSV =
    input.split(scala.util.Properties.lineSeparator).toList.map(_.split(",").toList)

  // Sample contents of a csv file containing person information
  val csvFile = """
    |Anne,35,1,Old Kent Road,
    |Bob,45,2,Whitechapel,
    |Charlie,20,,Lost,
    """.trim.stripMargin

  // Parse it into our CSV type
  val parsedCsv: CSV = parse(csvFile)
}
```

```scala
import CSVDefinitions._
import sqlest.extractor.{ CellExtractor, ExtractorSyntax }
```

Implement the CellExtractor trait for CSVRow that will read a String
```scala
case class StringExtractor(index: Int) extends CellExtractor[CSVRow, String] {
  def read(row: CSVRow): Option[String] = {
    val cellValue = row(index)
    if (cellValue.trim.nonEmpty) Some(cellValue)
    else None
  }
}
```

Implement the CellExtractor trait for CSVRow that will read a Int
```scala
case class IntExtractor(index: Int) extends CellExtractor[CSVRow, Int] {
  def read(row: CSVRow): Option[Int] = scala.util.Try(Integer.parseInt(row(index))).toOption
}
```

Extend the application with ExtractorSyntax for the row type.
This provides the methods `extract`, `extractTuple` and `extractConstant`.
```scala
object CSVApp extends ExtractorSyntax[CSVRow] {
  // Create some cell extractors
  val nameExtractor = StringExtractor(0)
  val ageExtractor = IntExtractor(1)
  val houseExtractor = IntExtractor(2)
  val streetExtractor = StringExtractor(3)

  // Create a tuple extractor that will read all fields
  val tupleExtractor =
    extractTuple(
      nameExtractor,
      ageExtractor,
      houseExtractor.asOption,
      streetExtractor.asOption)

  // Define the domain classes to extract from the CSV data
  case class Person(name: String, age: Int, address: Option[Address])
  case class Address(house: Int, street: String)

  // Create extractors to read the domain classes
  val addressExtractor = extract[Address](houseExtractor, streetExtractor)

  // Named arguments often enhance the readability of extractor definitions
  val personExtractor = extract[Person](
    name = nameExtractor,
    age = ageExtractor,
    address = addressExtractor.asOption)

  // `extractHeadOption` and `extractAll` must be passed an Iterable[CSVRow]
  // `List` implements `Iterable` so the parsedCsv can be used directly
  def tupleHeadOption = tupleExtractor.extractHeadOption(parsedCsv)
  def tupleAll = tupleExtractor.extractAll(parsedCsv)
  def addressHeadOption = addressExtractor.extractHeadOption(parsedCsv)
  def personAll = personExtractor.extractAll(parsedCsv)
}
```

`extractHeadOption` tries to read the first row. If there isn't one it returns None
```scala
scala> CSVApp.tupleHeadOption
res0: Option[(String, Int, Option[Int], Option[String])] = Some((Anne,35,Some(1),Some(Old Kent Road)))

scala> CSVApp.addressHeadOption
res1: Option[CSVApp.Address] = Some(Address(1,Old Kent Road))
```

`extractAll` reads all rows into a List
```scala
scala> CSVApp.tupleAll
res2: List[(String, Int, Option[Int], Option[String])] = List((Anne,35,Some(1),Some(Old Kent Road)))

scala> CSVApp.personAll
res3: List[CSVApp.Person] = List(Person(Anne,35,Some(Address(1,Old Kent Road))))
```

## More examples
See the tests for more examples of:
- ConstantExtractor
- OptionExtractor
- TupleExtractor
- MappedExtractor
- SeqExtractor
- ListMultiRowExtractor
- GroupedExtractor

## Suggested usages
- CSVs
- java.util.ResultSet - this is what [sqlest](https://github.com/jhc-systems/sqlest) uses extractors for
- Fixed format record data
- Converting a list of Tuples into case classes

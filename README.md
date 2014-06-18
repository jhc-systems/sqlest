# Sqlest

**sqlest** is a database library for Scala. It allows you to write SQL directly in Scala with type safety guarantees while also providing a simple mechanism of extracting domain specific case classes from the results. 

[![Build Status](https://travis-ci.org/jhc-systems/sqlest.svg?branch=master)](https://travis-ci.org/jhc-systems/sqlest)

## Using sqlest
Binary release artefacts are published to the Sonatype OSS Repository Hosting service and synced to Maven Central. Snapshots of master are built using Travis CI and automatically published to the Sonatype OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add:

```scala
resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)
```

To include sqlest in your project add the following library dependency:

```scala
libraryDependencies ++= Seq(
  "co.uk.jhc" %% "sqlest" % "0.1.0"
)
```

sqlest is only built with Scala 2.11

## Authors
- Dave Gurnell
- Brendan Maginnis

## Acknowledgements
- Dean Chapman - author of Sqler which inspired sqlest
- Frank Wallis - ideas, feedback and contributions
- [Slick](https://github.com/slick/slick) - a great project which contains many ideas used in sqlest
- [jOOQ](https://github.com/jOOQ/jOOQ) - a similar project written in Java which provided many ideas used in sqlest

package sqlest.examples

import sqlest._

trait DatabaseExample {
  // Configure a DataSource
  val dataSource = {
    val dataSource = new org.h2.jdbcx.JdbcDataSource
    dataSource.setURL("jdbc:h2:./test")
    dataSource
  }

  // Choose the StatementBuilder that is compatible with the database you are using
  val statementBuilder = sqlest.sql.H2StatementBuilder

  // Use the DataSource and the StatementBuilder to create an implicit database
  // This database is used in all execute calls
  implicit val database = Database.withDataSource(dataSource, statementBuilder)

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

  def executeRawSql(sql: String) =
    database.executeWithConnection { connection =>
      try {
        connection.createStatement.execute(sql)
      } catch {
        case e: Exception => // Ignore exception from dropping tables
      }
    }
}

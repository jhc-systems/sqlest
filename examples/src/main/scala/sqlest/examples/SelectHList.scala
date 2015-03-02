package sqlest.examples

import shapeless._
import shapeless.ops.hlist._
import sqlest._
import sqlest.ast._
import sqlest.extractor._

case class HListExtractor[Row, L <: HList, O <: HList](extractors: L)(implicit comapped: Comapped.Aux[L, TableColumn, O]) extends ProductExtractor[Row, O] {
  type Accumulator
  def accumulate(accumulator: Accumulator, row: Row): Accumulator = ???
  def emit(accumulator: Accumulator): Option[O] = ???
  def initialize(row: Row): Accumulator = ???
  def innerExtractors: List[sqlest.extractor.Extractor[Row, _]] = ???
}

object SelectHListExample extends App with DatabaseExample {
  InsertExamples.insertAll

  implicit def hlistExtractable[Row, L <: HList, O <: HList](implicit comapped: Comapped.Aux[L, TableColumn, O]): Extractable.Aux[Row, L, O] =
    new Extractable[Row, L] {
      type Out = O
      def extractor(l: L) = HListExtractor(l)
    }

  select(SmoothyTable.description :: FruitTable.name :: FruitTable.juiciness :: HNil)
    .from(SmoothyTable)
    .innerJoin(IngredientsTable).on(SmoothyTable.id === IngredientsTable.smoothyId)
    .innerJoin(FruitTable).on(IngredientsTable.fruitId === FruitTable.id)
    .where(SmoothyTable.description === "Magic dream shake")
    .fetchHeadOption //(hlistExtractable[java.sql.ResultSet, TableColumn[String] :: TableColumn[String] :: TableColumn[Int] :: HNil, String :: String :: Int :: HNil])
}


package aoc22

import cats.effect.Sync
import cats.parse.Numbers
import cats.syntax.apply._
import cats.syntax.functor._

class Aoc4[F[_]: Sync] extends Day.N[F]( 4 ) {
  import Aoc4._

  private def readRanges( live: Boolean ): fs2.Stream[F, ( Range, Range )] =
    lines( live )
      .through( Data.parseLines( parsers.ranges ) )

  override def basic( live: Boolean ): F[String] =
    readRanges( live )
      .map { case ( r1, r2 ) => if (r1.includes( r2 ) || r2.includes( r1 )) 1 else 0 }
      .compile
      .foldMonoid
      .map( _.toString )

  override def bonus( live: Boolean ): F[String] =
    readRanges( live )
      .map { case ( r1, r2 ) => if (r1.overlaps( r2 )) 1 else 0 }
      .compile
      .foldMonoid
      .map( _.toString )
}

object Aoc4 {
  import cats.parse.Parser

  object parsers {
    val id: Parser[Int] =
      Numbers.nonNegativeIntString.mapFilter( _.toIntOption )

    val range: Parser[Range] = ( id <* Parser.char( '-' ), id ).mapN( Range )

    val ranges: Parser[( Range, Range )] = (range <* Parser.char( ',' )) ~ range
  }

  case class Range( min: Int, max: Int ) {
    def includes( other: Range ): Boolean =
      min <= other.min && max >= other.max

    def overlaps( other: Range ): Boolean =
      !(min > other.max || max < other.min)
  }

}

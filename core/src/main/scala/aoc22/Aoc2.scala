package aoc22

import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry

class Aoc2[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {

  private def runWithScoring( p: Parser[Int] ): F[String] =
    lines
      .through( parseLines( p ) )
      .compile
      .foldMonoid
      .map( _.toString )

  override def basic: F[String] =
    runWithScoring( Aoc2.parsers.score )

  override def bonus: F[String] =
    runWithScoring( Aoc2.parsers.realScore )
}

object Aoc2 {
  object parsers {
    import cats.parse.Parser
    import cats.parse.Rfc5234.wsp

    val theirMove: Parser[Move] =
      Move.values.foldMapK( m => Parser.char( m.theirKey ).as( m ) )

    val ourMove: Parser[Move] =
      Move.values.foldMapK( m => Parser.char( m.ourKey ).as( m ) )

    val ourGoal: Parser[Outcome] =
      Outcome.values.foldMapK( o => Parser.char( o.key ).as( o ) )

    val movesLine: Parser[( Move, Move )] = (theirMove <* wsp) ~ ourMove

    val moveGoalLine: Parser[( Move, Outcome )] = (theirMove <* wsp) ~ ourGoal

    val score: Parser[Int] = movesLine.map {
      case ( their, our ) =>
        Move.duel( their, our ).value + our.value
    }

    val realScore: Parser[Int] = moveGoalLine.map {
      case ( their, outcome ) =>
        val our = Move.goal( their, outcome )

        our.value + outcome.value
    }
  }

  sealed abstract class Move( val value: Int, val theirKey: Char, val ourKey: Char ) extends EnumEntry

  object Move extends Enum[Move] {
    import Outcome._
    def duel( theirs: Move, ours: Move ): Outcome =
      ( theirs, ours ) match {
        case ( Rock, Rock )         => Draw
        case ( Rock, Paper )        => Win
        case ( Rock, Scissors )     => Loss
        case ( Paper, Rock )        => Loss
        case ( Paper, Paper )       => Draw
        case ( Paper, Scissors )    => Win
        case ( Scissors, Rock )     => Win
        case ( Scissors, Paper )    => Loss
        case ( Scissors, Scissors ) => Draw
      }

    def goal( theirs: Move, outcome: Outcome ): Move =
      ( theirs, outcome ) match {
        case ( Rock, Loss )     => Scissors
        case ( Rock, Draw )     => Rock
        case ( Rock, Win )      => Paper
        case ( Paper, Loss )    => Rock
        case ( Paper, Draw )    => Paper
        case ( Paper, Win )     => Scissors
        case ( Scissors, Loss ) => Paper
        case ( Scissors, Draw ) => Scissors
        case ( Scissors, Win )  => Rock
      }

    case object Rock     extends Move( 1, 'A', 'X' )
    case object Paper    extends Move( 2, 'B', 'Y' )
    case object Scissors extends Move( 3, 'C', 'Z' )

    override val values: Vector[Move] = findValues.toVector
  }

  sealed abstract class Outcome( val value: Int, val key: Char ) extends EnumEntry
  object Outcome extends Enum[Outcome] {
    case object Loss extends Outcome( 0, 'X' )
    case object Draw extends Outcome( 3, 'Y' )
    case object Win  extends Outcome( 6, 'Z' )

    override val values: Vector[Outcome] = findValues.toVector
  }

}

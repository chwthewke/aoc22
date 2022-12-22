package aoc22

import cats.Group
import cats.effect.Sync
import cats.kernel.CommutativeGroup
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import fs2.Stream

class Aoc9[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc9._

  def getDirections: Stream[F, Direction] =
    lines
      .through( parseLines( parsers.move ) )
      .flatMap( m => Stream.emits( Vector.fill( m.distance )( m.direction ) ) )

  override def basic: F[String] =
    getDirections
      .scan( Rope.init )( _.moveHead( _ ).updateTail )
      .map( _.tail )
      .compile
      .to( Set )
      .map( _.size.toString )

  override def bonus: F[String] =
    getDirections
      .scan( LongRope.init )( _.moveHead( _ ).updateTails )
      .map( _.tails.last )
      .compile
      .to( Set )
      .map( _.size.toString )
}

object Aoc9 {
  sealed abstract class Direction( val dx: Int, val dy: Int, val key: Char ) extends EnumEntry with Product

  object Direction extends Enum[Direction] {
    case object Left  extends Direction( -1, 0, 'L' )
    case object Right extends Direction( 1, 0, 'R' )
    case object Up    extends Direction( 0, 1, 'U' )
    case object Down  extends Direction( 0, -1, 'D' )

    override val values: Vector[Direction] = findValues.toVector
  }

  case class Move( direction: Direction, distance: Int )

  case class Position( x: Int, y: Int ) {
    def move( direction: Direction ): Position =
      Position( x + direction.dx, y + direction.dy )

    def norm: Int = // L-∞
      x.abs max y.abs

    def moveTowards( goal: Position ): Position = {
      val toGoal: Position = goal.remove( this )
      if (toGoal.norm <= 1)
        this
      else {
        val move = Position( toGoal.x.sign, toGoal.y.sign )
        Group.combine( this, move )
      }
    }

  }

  object Position {
    implicit val positionGroup: CommutativeGroup[Position] = new CommutativeGroup[Position] {
      override def inverse( a: Position ): Position = Position( -a.x, -a.y )

      override def empty: Position = Position( 0, 0 )

      override def combine( a: Position, b: Position ): Position = Position( a.x + b.x, a.y + b.y )
    }
  }

  case class Rope( head: Position, tail: Position ) {
    def moveHead( direction: Direction ): Rope = copy( head = head.move( direction ) )
    def updateTail: Rope                       = copy( tail = tail.moveTowards( head ) )
  }

  object Rope {
    val init: Rope = Rope( Position( 0, 0 ), Position( 0, 0 ) )
  }

  case class LongRope( head: Position, tails: Vector[Position] ) {
    def moveHead( direction: Direction ): LongRope = copy( head = head.move( direction ) )
    def updateTails: LongRope =
      copy( tails = tails.foldLeft( Vector( head ) )( ( acc, tail ) => acc :+ tail.moveTowards( acc.last ) ).tail )
  }

  object LongRope {
    val init: LongRope = LongRope( Position( 0, 0 ), Vector.fill( 9 )( Position( 0, 0 ) ) )
  }

  object parsers {
    import cats.parse.Rfc5234.wsp
    import cats.parse.Numbers

    val direction: Parser[Direction] =
      Direction.values
        .foldMapK( dir => Parser.char( dir.key ).as( dir ) )

    val move: Parser[Move] =
      ( direction <* wsp, Numbers.nonNegativeIntString.mapFilter( _.toIntOption ) ).mapN( Move )
  }
}

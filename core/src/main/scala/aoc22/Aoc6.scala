package aoc22

import cats.effect.Sync
import cats.syntax.either._

class Aoc6[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  def uniquePacket( signal: String, from: Int, window: Int ): Option[Int] =
    ((from + window) to signal.length)
      .find( x => signal.substring( x - window, x ).toSet.size == window )

  private def runDetector( detector: String => Option[Int] ): F[String] =
    lines
      .evalMap( signal => detector( signal ).toRight( Error( s"Marker not found: $signal" ) ).liftTo[F] )
      .map( _.toString )
      .intersperse( "\n" )
      .compile
      .foldMonoid

  override def basic: F[String] =
    runDetector( uniquePacket( _, 0, 4 ) )

  override def bonus: F[String] =
    runDetector( uniquePacket( _, 0, 14 ) )
}

package aoc22

import cats.effect.Sync
import cats.syntax.either._

class Aoc6[F[_]: Sync] extends Day.N[F]( 6 ) {
  def uniquePacket( signal: String, from: Int, window: Int ): Option[Int] =
    ((from + window) to signal.length)
      .find( x => signal.substring( x - window, x ).toSet.size == window )

  private def runDetector( live: Boolean, detector: String => Option[Int] ): F[String] =
    lines( live )
      .evalMap( signal => detector( signal ).toRight( Error( s"Marker not found: $signal" ) ).liftTo[F] )
      .map( _.toString )
      .intersperse( "\n" )
      .compile
      .foldMonoid

  override def basic( live: Boolean ): F[String] =
    runDetector( live, uniquePacket( _, 0, 4 ) )

  override def bonus( live: Boolean ): F[String] =
    runDetector( live, uniquePacket( _, 0, 14 ) )
}

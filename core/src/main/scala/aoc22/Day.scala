package aoc22

import cats.effect.Sync
import fs2.Stream

trait Day[F[_]] {
  def basic( live: Boolean ): F[String]
  def bonus( live: Boolean ): F[String]
}

object Day {
  abstract class N[F[_]: Sync]( val n: Int ) extends Day[F] {
    protected def lines( live: Boolean ): Stream[F, String] =
      Data
        .lines[F]( n, live )
        .map( _.trim )
        .filter( _.nonEmpty )
  }
}

package aoc22

import cats.effect.Sync
import cats.syntax.all._
import cats.parse.Parser
import fs2.Pipe
import fs2.Stream
import fs2.io
import fs2.text
import fs2.text.utf8

trait Day[F[_]] {
  def basic: F[String]
  def bonus: F[String]
}

object Day {
  abstract class Of[F[_]: Sync]( srcFile: String ) extends Day[F] {

    final def parseLines[A]( lineParser: Parser[A] ): Pipe[F, String, A] =
      _.evalMap( line => lineParser.parseAll( line ).leftMap( err => Error( formatError( line, err ) ) ).liftTo[F] )

    final def rawLines: Stream[F, String] =
      io.readInputStream(
          Sync[F].delay( getClass.getClassLoader.getResourceAsStream( srcFile ) ),
          1048576
        )
        .through( utf8.decode[F] )
        .through( text.lines[F] )

    final def lines: Stream[F, String] =
      rawLines.map( _.trim ).filter( _.nonEmpty )
  }

}

package aoc22

import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.either._
import fs2.Pipe
import fs2.Stream
import fs2.io.readInputStream
import fs2.text
import fs2.text.utf8

object Data {
  private def resource( day: Int, live: Boolean ): String =
    s"${if (live) "live" else "samples"}/$day.txt"

  private def bytes[F[_]: Sync]( day: Int, live: Boolean ): Stream[F, Byte] =
    readInputStream(
      Sync[F].delay( getClass.getClassLoader.getResourceAsStream( resource( day, live ) ) ),
      65536
    )

  def lines[F[_]: Sync]( day: Int, live: Boolean ): Stream[F, String] =
    bytes( day, live ).through( utf8.decode[F] ).through( text.lines[F] )

  def formatError( src: String, err: Parser.Error ): String =
    s"Error parsing '$src': $err"

  def parseLines[F[_]: Sync, A]( lineParser: Parser[A] ): Pipe[F, String, A] =
    _.evalMap( line => lineParser.parseAll( line ).leftMap( err => Error( formatError( line, err ) ) ).liftTo[F] )

}

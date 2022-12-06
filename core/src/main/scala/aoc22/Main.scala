package aoc22

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    IO( println( s"${buildinfo.Aoc22.name} ${buildinfo.Aoc22.version}" ) )
      .as( ExitCode.Success )

}

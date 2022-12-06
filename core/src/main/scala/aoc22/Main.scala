package aoc22

import cats.effect.ExitCode
import cats.effect.IO
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object Main extends CommandIOApp( "aoc2022", "Advent of Code 2022" ) {
  override def main: Opts[IO[ExitCode]] = Days.program[IO]
}

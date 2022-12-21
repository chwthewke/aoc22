package aoc22

import cats.effect.Clock
import cats.effect.ExitCode
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import com.monovore.decline.Command
import com.monovore.decline.Opts
import scala.collection.immutable.SortedMap

object Days {

  def days[F[_]: Sync]: Map[Int, Day[F]] = SortedMap(
    1 -> new Aoc1[F],
    2 -> new Aoc2[F],
    3 -> new Aoc3[F],
    4 -> new Aoc4[F],
    5 -> new Aoc5[F],
    6 -> new Aoc6[F],
    7 -> new Aoc7[F],
    8 -> new Aoc8[F]
  )

  private val liveOpt: Opts[Boolean] = Opts.flag( "live", "Use the live data", "l" ).orFalse

  private def commands[F[_]: Sync]: Opts[Boolean => F[String]] = {

    def basicCommand( dayNum: Int, day: Day[F] ): Opts[Boolean => F[String]] =
      Opts
        .subcommand( Command( s"$dayNum", s"Basic problem for day $dayNum" )( Opts.unit ) )
        .as( day.basic )

    def bonusCommand( dayNum: Int, day: Day[F] ): Opts[Boolean => F[String]] =
      Opts
        .subcommand( Command( s"$dayNum+", s"Bonus problem for day $dayNum" )( Opts.unit ) )
        .as( day.bonus )

    days[F].toVector.foldMap {
      case ( dayNum, day ) => Vector( basicCommand( dayNum, day ), bonusCommand( dayNum, day ) )
    }.foldK
  }

  def program[F[_]: Sync: Clock]: Opts[F[ExitCode]] =
    ( liveOpt, commands[F] ).mapN( ( live, p ) => run( p( live ) ) )

  private def run[F[_]: Sync: Clock]( program: F[String] ): F[ExitCode] = {

    val console: Console[F] = Console.make[F]

    for {
      s   <- Clock[F].monotonic
      res <- program.attempt
      e   <- Clock[F].monotonic
      _   <- console.errorln( show"[${(e - s).toMillis} ms]" )
      code <- res.fold(
               err => console.errorln( err.getMessage ).as( ExitCode.Error ),
               str => console.println( str ).as( ExitCode.Success )
             )
    } yield code

  }

}

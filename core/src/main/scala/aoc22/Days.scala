package aoc22

import cats.effect.ExitCode
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monadError._
import com.monovore.decline.Command
import com.monovore.decline.Opts

trait Day[F[_]] {
  def basic( live: Boolean ): F[String]
  def bonus( live: Boolean ): F[String]
}

object Days {

  def days[F[_]: Sync]: Map[Int, Day[F]] = Map(
    1 -> new Aoc1[F]
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

  def program[F[_]: Sync]: Opts[F[ExitCode]] =
    ( liveOpt, commands[F] ).mapN( ( live, p ) => handleError( p( live ).flatMap( Console.make[F].println ) ) )

  private def handleError[F[_]: Sync]( prog: F[Unit] ): F[ExitCode] = {
    prog.redeemWith(
      err => Console.make[F].errorln( err.getMessage ).as( ExitCode.Error ),
      Function.const( ExitCode.Success.pure[F] )
    )
  }

}

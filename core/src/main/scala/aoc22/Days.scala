package aoc22

import cats.effect.Clock
import cats.effect.ExitCode
import cats.effect.Sync
import cats.effect.std.Console
import cats.syntax.all._
import com.monovore.decline.Command
import com.monovore.decline.Opts

object Days {

  class Desc[F[_]](
      val dayNum: Int,
      val day: (String, Boolean) => Day[F]
  ) {

    protected def resource( live: Boolean, alt: Boolean ): String = {
      val f = if (alt && !live) s"${dayNum}a" else dayNum.toString

      s"${if (live) "live" else "samples"}/$f.txt"
    }

    def run( alt: Boolean ): Boolean => Day[F] =
      (live: Boolean) => day( resource( live, alt ), live )

    protected def mkCommand( cmd: String, help: String ): Opts[Boolean => Day[F]] =
      Opts
        .subcommand( Command( cmd, help )( Opts.unit ) )
        .as( run( alt = false ) )

    final def basicCommand: Opts[Boolean => F[String]] =
      mkCommand( s"$dayNum", s"Basic problem for day $dayNum" )
        .map( _.andThen( _.basic ) )

    final def bonusCommand: Opts[Boolean => F[String]] =
      mkCommand( s"$dayNum+", s"Bonus problem for day $dayNum" )
        .map( _.andThen( _.bonus ) )

    def commands: Opts[Boolean => F[String]] =
      basicCommand <+> bonusCommand
  }

  class DescAlt[F[_]]( dayNum0: Int, day0: (String, Boolean) => Day[F] ) extends Desc[F]( dayNum0, day0 ) {
    override protected def mkCommand( cmd: String, help: String ): Opts[Boolean => Day[F]] =
      Opts
        .subcommand( Command( cmd, help )( altOpt ) )
        .map( run )
  }

  object Desc {
    def apply[F[_]]( dayNum: Int, mkDay: (String, Boolean) => Day[F] ): Desc[F] =
      new Desc( dayNum, mkDay )
    def alt[F[_]]( dayNum: Int, mkDay: (String, Boolean) => Day[F] ): Desc[F] =
      new DescAlt[F]( dayNum, mkDay )
  }

  def days[F[_]: Sync]: Vector[Desc[F]] = Vector(
    Desc( 1, new Aoc1[F]( _, _ ) ),
    Desc( 2, new Aoc2[F]( _, _ ) ),
    Desc( 3, new Aoc3[F]( _, _ ) ),
    Desc( 4, new Aoc4[F]( _ , _) ),
    Desc( 5, new Aoc5[F]( _ , _) ),
    Desc( 6, new Aoc6[F]( _ , _) ),
    Desc( 7, new Aoc7[F]( _ , _) ),
    Desc( 8, new Aoc8[F]( _ , _) ),
    Desc.alt( 9, new Aoc9[F]( _, _ ) ),
    Desc( 10, new Aoc10[F]( _ , _) ),
    Desc( 11, new Aoc11[F]( _ , _) ),
    Desc( 12, new Aoc12[F]( _ , _) ),
    Desc( 13, new Aoc13[F]( _ , _) ),
    Desc( 14, new Aoc14[F]( _ , _) )
  )

  private val liveOpt: Opts[Boolean] = Opts.flag( "live", "Use the live data", "l" ).orFalse

  private val altOpt: Opts[Boolean] = Opts.flag( "alt", "Use alt sample (ignored if --live)", "a" ).orFalse

  private def commands[F[_]: Sync]: Opts[Boolean => F[String]] =
    days[F].foldMapK( _.commands )

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

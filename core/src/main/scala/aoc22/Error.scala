package aoc22

import cats.Applicative
import cats.Reducible
import cats.Show
import cats.data.NonEmptyChain
import cats.kernel.Semigroup
import cats.syntax.all._
import fs2.Stream
import fs2.Pure

final case class Error( errs: NonEmptyChain[( String, Option[Throwable] )] )
    extends Throwable( "", null, false, false ) {
  override def getMessage: String = Error.show( this )

  def traverse[F[_]: Applicative, A]( f: ( String, Option[Throwable] ) => F[A] ): F[NonEmptyChain[A]] =
    errs.traverse( f.tupled )

  def traverse_[F[_]: Applicative, A]( f: ( String, Option[Throwable] ) => F[A] ): F[Unit] =
    errs.traverse_( f.tupled )
}

object Error {
  private def mkError( message: String, exn: Option[Throwable] ): Error =
    Error( NonEmptyChain.one( ( message, exn ) ) )

  def apply( message: String, exn: Throwable ): Error =
    mkError( message, Some( exn ) )

  def apply( exn: Throwable ): Error = exn match {
    case e: Error => e
    case _        => mkError( s"${exn.getClass.getSimpleName} ${exn.getMessage}", Option( exn.getCause ) )
  }

  def apply[F[_]: Reducible, A <: Throwable]( exns: F[A] ): Error = exns.reduceMap( Error( _ ) )

  def apply( message: String ): Error = mkError( message, None )

  implicit val errorSemigroup: Semigroup[Error] =
    ( x, y ) => Error( x.errs ++ y.errs )

  private def subErrors(
      indent: Int,
      error: Error
  ): Stream[Pure, ( Int, String )] =
    Stream.unfold( error.errs.toChain )( _.uncons ).flatMap {
      case ( msg, None )                     => Stream.emit( ( indent, msg ) )
      case ( msg, Some( err @ Error( _ ) ) ) => subErrors( indent + 1, err ).cons1( ( indent, msg ) )
      case ( msg, Some( t ) )                => subErrors( indent + 1, Error( t ) ).cons1( ( indent, msg ) )
    }

  def show( error: Error ): String =
    subErrors( 0, error )
      .map { case ( ind, msg ) => s"${"  " * ind}$msg" }
      .intersperse( "\n" )
      .compile
      .foldMonoid

  implicit val showError: Show[Error] = Show.show( show )

  object one {
    def apply( t: Throwable ): Error = Error( t )
    def unapply( e: Error ): Option[Throwable] =
      if (e.errs.length == 1L) e.errs.head._2
      else None
  }

}

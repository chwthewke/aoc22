import cats.MonadThrow
import cats.parse.Parser
import cats.syntax.all._

package object aoc22 {
  implicit class EitherStringOps[A]( private val self: Either[String, A] ) {
    def into[F[_]: MonadThrow]: F[A] = self.leftMap( Error( _ ) ).liftTo[F]
  }

  final def formatError( src: String, err: Parser.Error ): String =
    s"Error parsing '$src': $err"
}

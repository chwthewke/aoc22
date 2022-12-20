import cats.MonadThrow
import cats.syntax.either._

package object aoc22 {
  implicit class EitherStringOps[A]( private val self: Either[String, A] ) {
    def into[F[_]: MonadThrow]: F[A] = self.leftMap( Error( _ ) ).liftTo[F]
  }
}

package aoc22

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.show._
import fs2.Stream

class Aoc1[F[_]: Sync] extends Day.N[F]( 1 ) {

  private def values( live: Boolean ): Stream[F, Option[Int]] =
    lines( live ).map( _.toIntOption )

  override def basic( live: Boolean ): F[String] =
    values( live )
      .fold( ( 0, 0 ) ) {
        case ( ( max, curr ), None )         => ( max.max( curr ), 0 )
        case ( ( max, curr ), Some( item ) ) => ( max, curr + item )
      }
      .map { case ( max, current ) => max.max( current ).show }
      .compile
      .lastOrError

  override def bonus( live: Boolean ): F[String] = {
    def push( list: NonEmptyList[Int] ): NonEmptyList[Int] =
      NonEmptyList( 0, list.sortBy( -_ ).take( 3 ) )

    values( live )
      .fold( NonEmptyList.one( 0 ) ) {
        case ( list, None )                               => push( list )
        case ( NonEmptyList( head, tail ), Some( item ) ) => NonEmptyList( head + item, tail )
      }
      .map( push )
      .map( _.reduce.show )
      .compile
      .lastOrError
  }
}

case class Elves( max: Int, current: Int )
object Elves { val empty: Elves = Elves( 0, 0 ) }

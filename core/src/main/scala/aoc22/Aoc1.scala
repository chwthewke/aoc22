package aoc22

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all._
import fs2.Stream

class Aoc1[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {

  private def values: Stream[F, Option[Int]] =
    lines.map( _.toIntOption )

  override def basic: F[String] =
    values
      .fold( ( 0, 0 ) ) {
        case ( ( max, curr ), None )         => ( max.max( curr ), 0 )
        case ( ( max, curr ), Some( item ) ) => ( max, curr + item )
      }
      .map { case ( max, current ) => max.max( current ).show }
      .compile
      .lastOrError

  override def bonus: F[String] = {
    def push( list: NonEmptyList[Int] ): NonEmptyList[Int] =
      NonEmptyList( 0, list.sortBy( -_ ).take( 3 ) )

    values
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

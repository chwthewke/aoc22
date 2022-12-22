package aoc22

import cats.effect.Sync
import cats.syntax.all._
import fs2.Chunk
import mouse.boolean._

class Aoc3[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  def getPriority( c: Char ): Either[String, Int] =
    priorities.get( c ).toRight( s"Invalid item char $c" )

  private val priorities: Map[Char, Int] =
    (
      ('a' to 'z').zipWithIndex.map { case ( c, n )   => ( c, n + 1 ) } ++
        ('A' to 'Z').zipWithIndex.map { case ( c, n ) => ( c, n + 27 ) }
    ).toMap

  private def contents( compartment: String ): Set[Char] = compartment.toSet

  private def uniqueElement[A]( set: Set[A] ): Either[String, A] =
    if (set.size == 1) Right( set.head )
    else Left( s"Invalid intersect ${set.mkString}" )

  private def identifyMispacked( sack: String ): F[Int] = {
    val result: Either[String, Int] =
      for {
        half <- (sack.length % 2 == 0).either( s"Invalid sack size ${sack.length}", sack.length / 2 )
        commonItems = contents( sack.take( half ) ).intersect( contents( sack.drop( half ) ) )
        item     <- uniqueElement( commonItems )
        priority <- getPriority( item )
      } yield priority

    result
      .leftMap( err => Error( s"Error with sack $sack: $err" ) )
      .liftTo[F]
  }

  override def basic: F[String] = {
    lines
      .evalMap( identifyMispacked )
      .compile
      .foldMonoid
      .map( _.toString )
  }

  def groupBadge( sacks: Chunk[String] ): F[Int] = {
    val result = for {
      _ <- (sacks.size == 3).either( s"Invalid group size ${sacks.size}", () )
      commonItems = sacks( 0 ).toSet.intersect( sacks( 1 ).toSet ).intersect( sacks( 2 ).toSet )
      badge    <- uniqueElement( commonItems )
      priority <- getPriority( badge )
    } yield priority

    result
      .leftMap( err => Error( s"Error with sacks ${sacks.mkString_( ", " )}: $err" ) )
      .liftTo[F]
  }

  override def bonus: F[String] =
    lines
      .chunkN( 3, allowFewer = true )
      .evalMap( groupBadge )
      .compile
      .foldMonoid
      .map( _.toString )
}

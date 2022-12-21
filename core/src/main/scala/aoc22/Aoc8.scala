package aoc22

import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import scala.annotation.tailrec

class Aoc8[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc8._

  def getGrid: F[Grid] =
    lines
      .evalScan( GridAccum( None, Vector.empty ) )(
        ( acc, line ) => acc.readLine( line ).into[F]
      )
      .compile
      .lastOrError
      .flatMap( acc => acc.validate.into[F] )

  override def basic: F[String] =
    getGrid
      .map( _.visibleTrees.toString )

  override def bonus: F[String] =
    getGrid
      .flatMap( _.bestViewDist.into[F] )
      .map( _.toString )
}

object Aoc8 {
  case class Grid( width: Int, height: Int, trees: Vector[Vector[Int]] ) {
    def visibleTrees: Int =
      ( 0.until( width ).toVector, 0.until( height ).toVector )
        .mapN( isVisible )
        .count( identity )

    private def isVisible( x: Int, y: Int ): Boolean =
      x == 0 ||
        y == 0 ||
        x + 1 == width ||
        y + 1 == height || {
        val t = trees( y )( x )
        isVisibleFromLeft( x, y, t ) ||
        isVisibleFromRight( x, y, t ) ||
        isVisibleFromTop( x, y, t ) ||
        isVisibleFromBottom( x, y, t )
      }

    private def isVisibleFromLeft( x: Int, y: Int, t: Int ): Boolean =
      trees( y ).take( x ).forall( _ < t )

    private def isVisibleFromRight( x: Int, y: Int, t: Int ): Boolean =
      trees( y ).drop( x + 1 ).forall( _ < t )

    private def isVisibleFromTop( x: Int, y: Int, t: Int ): Boolean =
      trees.take( y ).forall( r => r( x ) < t )

    private def isVisibleFromBottom( x: Int, y: Int, t: Int ): Boolean =
      trees.drop( y + 1 ).forall( r => r( x ) < t )

    def viewDist( x: Int, y: Int ): Int = {
      val t = trees( y )( x )
      viewDistUp( x, y, t ) *
        viewDistDown( x, y, t ) *
        viewDistLeft( x, y, t ) *
        viewDistRight( x, y, t )
    }

    def bestViewDist: Either[String, Int] =
      ( 1.until( width - 1 ).toVector, 1.until( height - 1 ).toVector )
        .mapN( viewDist )
        .maxOption
        .toRight( "Grid smaller than 3x3" )

    @tailrec
    private def viewDistOf( acc: Int, los: Vector[Int], alt: Int, from: Int ): Int = {
      if (from >= los.size) acc
      else if (los( from ) < alt) viewDistOf( acc + 1, los, alt, from + 1 )
      else acc + 1
    }

    def viewDistUp( x: Int, y: Int, t: Int ): Int =
      viewDistOf( 0, (1 to y).map( j => trees( y - j )( x ) ).toVector, t, 0 )

    def viewDistDown( x: Int, y: Int, t: Int ): Int =
      viewDistOf( 0, ((y + 1) until height).map( j => trees( j )( x ) ).toVector, t, 0 )

    def viewDistLeft( x: Int, y: Int, t: Int ): Int =
      viewDistOf( 0, (1 to x).map( i => trees( y )( x - i ) ).toVector, t, 0 )

    def viewDistRight( x: Int, y: Int, t: Int ): Int =
      viewDistOf( 0, ((x + 1) until width).map( i => trees( y )( i ) ).toVector, t, 0 )
  }

  case class GridAccum( width: Option[Int], trees: Vector[Vector[Int]] ) {
    def readLine( line: String ): Either[String, GridAccum] =
      width
        .filter( _ != line.length )
        .map( w => s"Inconsistent line length (expected $w): $line" )
        .toLeft( () ) *>
        line.toVector
          .traverseWithIndexM {
            case ( c, ix ) =>
              c.toString.toIntOption.toRight( s"Unexpected $c at index $ix: $line" )
          }
          .map( treeLine => GridAccum( width.orElse( Some( treeLine.size ) ), trees :+ treeLine ) )

    def validate: Either[String, Grid] =
      width.toRight( "Empty grid" ).map( Grid( _, trees.size, trees ) )
  }

}

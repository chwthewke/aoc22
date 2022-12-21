package aoc22

import cats.Monad
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

class Aoc14[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc14._

  def getPaths: F[NonEmptyVector[Path]] =
    lines
      .through( parseLines( parsers.path ) )
      .compile
      .toVector
      .flatMap( v => NonEmptyVector.fromVector( v ).toRight( "No paths" ).into[F] )

  override def basic: F[String] =
    getPaths.map( Grid( _, 500 ) ).map( _.dropAll.toString )

  override def bonus: F[String] =
    getPaths.map( Grid.withFloor( _, 500 ) ).map( _.dropAll.toString )
}

object Aoc14 {

  case class Position( x: Int, y: Int )

  case class Path( positions: NonEmptyList[Position] )

  sealed abstract class Cell( val key: Char ) extends EnumEntry
  object Cell extends Enum[Cell] {
    case object Rock extends Cell( '#' )
    case object Void extends Cell( '.' )
    case object Sand extends Cell( 'o' )

    override val values: Vector[Cell] = findValues.toVector
  }

  sealed abstract class Drop[+A] extends Product
  case object Blocked            extends Drop[Nothing]
  case object FallOut            extends Drop[Nothing]
  case class Rest[A]( a: A )     extends Drop[A]

  object Drop {
    implicit def dropShow[A: Show]: Show[Drop[A]] = Show.show {
      case Blocked   => "Blocked"
      case FallOut   => "FallOut"
      case Rest( a ) => a.show
    }

    implicit val dropMonad: Monad[Drop] = new Monad[Drop] {
      override def pure[A]( x: A ): Drop[A] = Rest( x )

      override def flatMap[A, B]( fa: Drop[A] )( f: A => Drop[B] ): Drop[B] =
        fa match {
          case Blocked   => Blocked
          case FallOut   => FallOut
          case Rest( a ) => f( a )
        }

      @tailrec
      override def tailRecM[A, B]( a: A )( f: A => Drop[Either[A, B]] ): Drop[B] =
        f( a ) match {
          case Blocked => Blocked
          case FallOut => FallOut
          case Rest( a ) =>
            a match {
              case Left( a1 ) => tailRecM( a1 )( f )
              case Right( b ) => Rest( b )
            }
        }
    }
  }

  case class Grid(
      width: Int,
      xOffset: Int,
      source: Position,
      rows: Vector[Vector[Cell]]
  ) {
    val height: Int = rows.size
    val xMin: Int   = xOffset
    val xMax: Int   = xOffset + width - 1

    // unsafe, no bounds check
    private def cell( x: Int, y: Int ): Cell = rows( y )( x - xMin )

    def cellAt( x: Int, y: Int ): Option[Cell] =
      Option.when( x >= xMin && x <= xMax && y >= 0 && y < height )( cell( x, y ) )

    def update( position: Position, cell: Cell ): Grid =
      if (position.y < 0 || position.y >= rows.size || position.x < xOffset || position.x >= width + xOffset)
        this
      else
        copy( rows = rows.updated( position.y, rows( position.y ).updated( position.x - xOffset, cell ) ) )

    private def dropSandAtStraight( pos: Position ): Drop[Position] =
      if (pos.x < xMin || pos.x > xMax || pos.y >= height) FallOut
      else if (cell( pos.x, pos.y ) != Cell.Void) Blocked
      else
        (pos.y + 1)
          .until( height )
          .find( y => cell( pos.x, y ) != Cell.Void )
          .map( y => Rest( Position( pos.x, y - 1 ) ) )
          .getOrElse( FallOut )

    private def dropSandAt( pos: Position ): Drop[Position] =
      pos.tailRecM(
        p =>
          dropSandAtStraight( p ).map { next =>
            if (cellAt( next.x - 1, next.y + 1 ).forall( _ == Cell.Void ))
              Left( Position( next.x - 1, next.y + 1 ) )
            else if (cellAt( next.x + 1, next.y + 1 ).forall( _ == Cell.Void ))
              Left( Position( next.x + 1, next.y + 1 ) )
            else Right( next )
          }
      )

    def dropSand: Drop[Grid] =
      dropSandAt( source ).map( pos => update( pos, Cell.Sand ) )

    def dropAll: Int = Grid.dropLoop( this, 0 )

    def render: String = {
      val leftW: Int = (height - 1).toString.length

      val notable: Set[Int] = Set( xMin, source.x, xMax )

      def digit( i: Int, x: Int ): Char = {
        val s = x.toString
        s.lift( s.length - i ).getOrElse( ' ' )
      }

      def topRow( i: Int ): String =
        " " * (leftW + 1) +
          (xMin to xMax).map( x => if (notable( x )) digit( i, x ) else ' ' ).mkString

      def cell( x: Int, y: Int ): Char =
        if (Position( x, y ) == source) '+'
        else rows( y )( x - xOffset ).key

      def gridRow( y: Int ): String =
        y.toString.reverse.padTo( leftW, ' ' ).reverse + " " +
          (xMin to xMax).map( cell( _, y ) ).mkString

      val topRowCount: Int = xMax.toString.length

      val textRows: Seq[String] =
        topRowCount.to( 1 ).by( -1 ).map( topRow ) ++ 0.until( height ).map( gridRow )

      textRows.mkString( "\n" )
    }
  }

  object Grid {
    implicit val gridShow: Show[Grid] = Show.show( _.render )

    @tailrec
    def dropLoop( grid: Grid, dropped: Int ): Int = {
      grid.dropSand match {
        case Blocked         => dropped
        case FallOut         => dropped
        case Rest( newGrid ) => dropLoop( newGrid, dropped + 1 )
      }
    }

    def apply( paths: NonEmptyVector[Path], sourceX: Int ): Grid = {
      val allPositions: NonEmptyList[Position] = paths.reduceMap( _.positions )
      val allXs: NonEmptyList[Int]             = sourceX :: allPositions.map( _.x )
      val xMin: Int                            = allXs.minimum
      val xMax: Int                            = allXs.maximum
      val yMax: Int                            = allPositions.map( _.y ).maximum

      val width: Int = xMax - xMin + 1
      val emptyGrid: Grid =
        Grid(
          width,
          xMin,
          Position( sourceX, 0 ),
          Vector.fill( yMax + 1 )( Vector.fill( width )( Cell.Void ) )
        )

      paths.foldLeft( emptyGrid )( addRockPath )
    }

    def withFloor( paths: NonEmptyVector[Path], sourceX: Int ): Grid = {
      val allPositions: NonEmptyList[Position] = paths.reduceMap( _.positions )
      val yMaxPath: Int                        = allPositions.map( _.y ).maximum
      // row (yMax + 1) is empty (this is where we are), and row (yMax + 2) is the floor
      val yFloor: Int = yMaxPath + 2

      // sand cannot fall farther laterally than [ sourceX - yMax, sourceX + yMax ]
      // that's because it needs to go down at least one cell in order to go sideways one cell
      // lets extend the floor 1 more cell on each side for superstition
      val floor: Path = Path(
        NonEmptyList.of( Position( sourceX - yFloor, yFloor ), Position( sourceX + yFloor, yFloor ) )
      )

      Grid( paths.append( floor ), sourceX )
    }

    private def range( i: Int, j: Int ): Range =
      i.min( j ) to i.max( j )

    private def addRockPathSegment( grid: Grid, start: Position, end: Position ): Grid = {
      val positions: IndexedSeq[Position] = for {
        x <- range( start.x, end.x )
        y <- range( start.y, end.y )
      } yield Position( x, y )

      positions.foldLeft( grid )( _.update( _, Cell.Rock ) )
    }

    private def addRockPath( grid: Grid, path: Path ): Grid =
      path.positions.toList.sliding2
        .foldLeft( grid ) { case ( acc, ( start, end ) ) => addRockPathSegment( acc, start, end ) }

  }

  object parsers {
    import cats.parse.Numbers.nonNegativeIntString
    import cats.parse.Rfc5234.wsp

    private val int: Parser[Int] =
      nonNegativeIntString.mapFilter( _.toIntOption )
    private val position: Parser[Position] =
      ( int <* Parser.char( ',' ), int ).mapN( Position )

    val path: Parser[Path] =
      position
        .repSep( wsp *> Parser.string( "->" ) <* wsp )
        .map( Path )
  }

}

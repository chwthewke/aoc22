package aoc22

import cats.effect.Sync
import cats.syntax.all._
import scala.annotation.tailrec

class Aoc12[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc12._
  def getGrid: F[Grid] =
    lines
      .evalScan( GridData.init )( ( d, l ) => d.readLine( l ).into[F] )
      .compile
      .lastOrError
      .flatMap( _.grid.into[F] )

  override def basic: F[String] =
    getGrid
      .flatMap( grid => grid.pathBasic.toRight( "No path found" ).into[F] )
      .map( _.length.toString )

  override def bonus: F[String] =
    getGrid
      .flatMap( grid => grid.pathBonus.toRight( "No path found" ).into[F] )
      .map( _.length.toString )
}

object Aoc12 {
  case class GridData(
      width: Option[Int],
      start: Option[Position],
      goal: Option[Position],
      lines: Vector[Vector[Int]]
  ) {
    private def validateWidth( line: String ): Either[String, Vector[Char]] =
      width
        .filter( _ != line.length )
        .map( w => s"Expected line of length $w: $line" )
        .toLeft( line.toVector )

    private def elevationOf( c: Char ): Either[String, Int] =
      Some( c - 'a' )
        .filter( e => e >= 0 && e <= 25 )
        .toRight( s"Invalid elevation $c" )

    def readChar( c: Char, x: Int ): Either[String, ( GridData, Int )] = {
      val cursor: Position = Position( x, lines.size )
      c match {
        case 'S' =>
          start
            .map( s => s"start redefined at $cursor, was $s" )
            .toLeft( ( copy( start = Some( cursor ) ), 0 ) )
        case 'E' =>
          goal
            .map( e => s"goal redefined at $cursor, was $e" )
            .toLeft( ( copy( goal = Some( cursor ) ), 25 ) )
        case c =>
          elevationOf( c ).tupleLeft( this )
      }
    }

    def readChars( line: Vector[Char] ): Either[String, GridData] =
      line.zipWithIndex
        .foldLeftM( ( this, Vector.empty[Int] ) ) {
          case ( ( grid, lineAcc ), ( c, x ) ) =>
            grid.readChar( c, x ).map { case ( g, e ) => ( g, lineAcc :+ e ) }
        }
        .map { case ( grid, line ) => grid.copy( width = width.orElse( Some( line.length ) ), lines = lines :+ line ) }

    def readLine( line: String ): Either[String, GridData] =
      for {
        chars   <- validateWidth( line )
        newGrid <- readChars( chars )
      } yield newGrid

    def grid: Either[String, Grid] =
      for {
        w        <- width.toRight( "Empty grid" )
        startPos <- start.toRight( "Start not defined" )
        goalPos  <- goal.toRight( "Goal not defined" )
      } yield Grid( w, startPos, goalPos, lines )
  }

  object GridData {
    val init: GridData = GridData( None, None, None, Vector.empty )
  }

  case class Position( x: Int, y: Int ) {
    def neighbours: Vector[Position] =
      Vector(
        Position( x - 1, y ),
        Position( x + 1, y ),
        Position( x, y - 1 ),
        Position( x, y + 1 )
      )
  }

  case class Path( positions: List[Position], length: Int ) {
    def append( p: Position ): Path = Path( p :: positions, length + 1 )
  }

  case class Grid(
      width: Int,
      start: Position,
      goal: Position,
      elevations: Vector[Vector[Int]]
  ) {
    val height: Int = elevations.size

    def elevationAt( pos: Position ): Int =
      elevations( pos.y )( pos.x )

    def valid( pos: Position ): Boolean =
      pos.x >= 0 && pos.x < width &&
        pos.y >= 0 && pos.y < height

    def neighbours( pos: Position ): Vector[Position] = {
      val elevation = elevationAt( pos )

      pos.neighbours.filter(
        p => valid( p ) && elevationAt( p ) >= elevation - 1
      )
    }

    def pathBasic: Option[Path] = path( _ == start )

    def pathBonus: Option[Path] = path( pos => elevationAt( pos ) == 0 )

    private def path( pred: Position => Boolean ): Option[Path] =
      loop( pred )( None, Map( goal -> Path( goal :: Nil, 0 ) ), Vector( goal ) )

    @tailrec
    private def loop(
        pred: Position => Boolean
    )( solved: Option[Path], seen: Map[Position, Path], open: Vector[Position] ): Option[Path] =
      solved match {
        case Some( p ) => Some( p )
        case None =>
          if (open.isEmpty)
            None
          else {
            val look = open.head

            val next = neighbours( look ).filterNot( seen.contains )
            val path = seen( look )

            val moreSeen = next.map( p => ( p, path.append( p ) ) )
            val newSeen  = seen ++ moreSeen
            val newOpen  = open.tail ++ next

            val newSolved = moreSeen.find { case ( pos, _ ) => pred( pos ) }.map( _._2 )

            loop( pred )( newSolved, newSeen, newOpen )
          }
      }

  }

}

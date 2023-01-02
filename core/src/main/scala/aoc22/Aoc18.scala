package aoc22

import cats.Monoid
import cats.Show
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.effect.Sync
import cats.kernel.Semigroup
import cats.parse.Parser
import cats.syntax.all._
import fs2.Stream
import scala.annotation.tailrec

class Aoc18[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc18._

  def cubes: Stream[F, Position] =
    lines.through( parseLines( parsers.position ) )

  override def basic: F[String] =
    cubes
      .scan( PositionScanner.init )( _.add( _ ) )
      .compile
      .lastOrError
      .map( _.surfaceArea.toString )

  def getCubes: F[NonEmptyVector[Position]] =
    cubes.compile.toVector
      .flatMap( _.toNev.toRight( "Empty positions" ).into[F] )

  def exteriorFaces( positions: NonEmptyVector[Position] ): String = {
    println( s"Starting with ${positions.length} cells" )
    val positionsSet: Set[Position] = positions.toVector.toSet
    val boundingBox: BoundingBox    = positions.reduceMap( BoundingBox( _ ) )
    println( s"Bounding Box $boundingBox" )
    val neighbours: Set[Position] = positions.reduceMap( p => p.faceOrEdgeNeighbours.filterNot( positionsSet ).toSet )
    println( s"${neighbours.size} neighbouring positions" )
    val components: Vector[Set[Position]] = connectedComponents( neighbours )
    println( s"${components.size} components (check: total size ${components
      .foldMap( _.size )}), sizes ${components.map( _.size ).mkString( ", " )}" )

    val exteriorComponents: Vector[Set[Position]] =
      components.filter( _.exists( boundingBox.outOfBounds ) )
    println( s"${exteriorComponents.size} components outside of bounding box" )

    val exteriorCells: Set[Position] = exteriorComponents.combineAll

    val exteriorFaces = positions.reduceMap( p => p.faceNeighbours.count( exteriorCells ) )

    exteriorFaces.toString
  }

  override def bonus: F[String] =
    getCubes.map( exteriorFaces )
}

object Aoc18 {
  def connectedComponents( positions: Set[Position] ): Vector[Set[Position]] =
    connectedComponentsLoop( Vector.empty, Set.empty, Vector.empty, positions )

  @tailrec
  private def connectedComponentsLoop(
      prev: Vector[Set[Position]],
      current: Set[Position],
      open: Vector[Position],
      source: Set[Position]
  ): Vector[Set[Position]] = {
    def pushCurrent: Vector[Set[Position]] =
      if (current.isEmpty) prev else prev :+ current

    if (source.isEmpty) pushCurrent
    else if (open.isEmpty)
      connectedComponentsLoop( pushCurrent, Set.empty, Vector( source.head ), source )
    else {
      val next = open.head
      if (current( next ))
        connectedComponentsLoop( prev, current, open.tail, source )
      else {
        val neighbours: Vector[Position] = next.faceNeighbours.filter( p => !current( p ) && source( p ) )
        connectedComponentsLoop( prev, current + next, open.tail ++ neighbours, source - next )
      }
    }
  }

  case class BoundingBox( min: Position, max: Position ) {
    def join( other: BoundingBox ): BoundingBox = BoundingBox( min.min( other.min ), max.max( other.max ) )

    def outOfBounds( position: Position ): Boolean =
      position.x < min.x ||
        position.x > max.x ||
        position.y < min.y ||
        position.y > max.y ||
        position.z < min.z ||
        position.z > max.z

    override def toString: String = s"$min -> $max"
  }

  object BoundingBox {
    implicit val boundingBoxSemigroup: Semigroup[BoundingBox] = Semigroup.instance( _.join( _ ) )
    implicit val boundingBoxShow: Show[BoundingBox]           = Show.fromToString[BoundingBox]

    def apply( position: Position ): BoundingBox = BoundingBox( position, position )
  }

  case class PositionScanner( positions: Set[Position], contacts: Int ) {
    def add( position: Position ): PositionScanner =
      if (positions.contains( position ))
        this
      else
        PositionScanner(
          positions + position,
          contacts + position.faceNeighbours.count( positions.contains )
        )

    def surfaceArea: Int =
      positions.size * 6 - 2 * contacts
  }

  object PositionScanner {
    val init: PositionScanner = PositionScanner( Set.empty, 0 )
  }

  case class Position( x: Int, y: Int, z: Int ) {
    def faceNeighbours: Vector[Position]       = Position.faceNeighbours.map( this |+| _ )
    def faceOrEdgeNeighbours: Vector[Position] = Position.faceOrEdgeNeighbours.map( this |+| _ )

    def min( other: Position ): Position = Position( x min other.x, y min other.y, z min other.z )
    def max( other: Position ): Position = Position( x max other.x, y max other.y, z max other.z )

    override def toString: String = s"( $x, $y, $z )"
  }

  object Position {
    private def adjacentToZero( order: Int ): Vector[Position] = {
      val ds = Vector( -1, 0, 1 )
      ( ds, ds, ds ).tupled.mapFilter {
        case ( dx, dy, dz ) => Option.when( dx.abs + dy.abs + dz.abs == order )( Position( dx, dy, dz ) )
      }
    }

    val faceNeighbours: Vector[Position]       = adjacentToZero( 1 )
    val edgeNeighbours: Vector[Position]       = adjacentToZero( 2 )
    val faceOrEdgeNeighbours: Vector[Position] = faceNeighbours ++ edgeNeighbours

    implicit val positionMonoid: Monoid[Position] = semiauto.monoid[Position]
    implicit val positionShow: Show[Position]     = Show.fromToString[Position]
  }

  object parsers {
    import cats.parse.Numbers.signedIntString

    private val coordinate: Parser[Int] = signedIntString.mapFilter( _.toIntOption )
    val position: Parser[Position] =
      (
        coordinate <* Parser.char( ',' ),
        coordinate <* Parser.char( ',' ),
        coordinate
      ).mapN( Position( _, _, _ ) )
  }

}

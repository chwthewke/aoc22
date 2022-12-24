package aoc22

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

class Aoc17[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc17._

  def initGrid: F[Grid] =
    lines
      .evalMap( l => liftF( parseLine( l ) ) )
      .compile
      .foldMonoid
      .map( jets => Grid.init( Piece.basicOrder, jets ) )

  @tailrec
  final def heightAfter(
      grid: Grid,
      numPieces: Int,
      nextIsFall: Boolean
  ): Int =
    if (grid.pieceCount > numPieces) grid.levels.size
    else if (nextIsFall) {
      heightAfter( grid.goDown, numPieces, nextIsFall = false )
    } else {
      heightAfter( grid.applyJet, numPieces, nextIsFall = true )
    }

  override def basic: F[String] =
    initGrid.flatMap( grid => Sync[F].blocking( BasicGridRunner( 2022L ).run( grid ).toString ) )

  override def bonus: F[String] =
    initGrid.flatMap( grid => Sync[F].blocking( PeriodSearchGridRunner( 1000000000000L, Nil ).run( grid ).toString ) )
}

object Aoc17 {
  sealed abstract class Jet( val key: Char ) extends EnumEntry
  object Jet extends Enum[Jet] {
    case object Left  extends Jet( '<' )
    case object Right extends Jet( '>' )

    override val values: Vector[Jet] = findValues.toVector
  }

  def parseLine( str: String ): Either[String, Vector[Jet]] =
    str.toVector.traverse( c => Jet.values.find( _.key == c ).toRight( s"Invalid character $c" ) )

  case class Row( b: Byte ) {
    def apply( n: Int ): Boolean =
      if (n < 0 || n > 6) false
      else getBit( n )

    def getBit( n: Int ): Boolean =
      (b & (1 << n)) != 0

    def lowSetBit: Option[Int]  = (0 to 6).find( getBit )
    def highSetBit: Option[Int] = (6 to 0 by (-1)).find( getBit )

    def shiftLeft: Row =
      if (getBit( 0 )) this else Row( (b >> 1).toByte )

    def shiftRight: Row =
      if (getBit( 6 )) this else Row( (b << 1).toByte )

    def intersect( other: Row ): Boolean =
      (b & other.b) != 0

    def merge( other: Row ): Row =
      Row( (b | other.b).toByte )

    def repr( c: Char ): String =
      (0 to 6).map( i => if (getBit( i )) c else '.' ).mkString
  }

  object Row {
    def apply( str: String ): Row =
      Row( str.zipWithIndex.toVector.foldMap { case ( c, i ) => (if (c == '#') 1 else 0) << i }.toByte )

    val empty: Row = Row( 0.toByte )
  }

  /**
    * Bottom-to-top rows representing a piece
    *
    * can be left/right shifted to change its x-position on the grid
    */
  case class Piece( rows: NonEmptyList[Row], lowX: Int, highX: Int ) {
    def shiftLeft: Piece =
      if (lowX == 0) this else Piece( rows.map( _.shiftLeft ), lowX - 1, highX - 1 )

    def shiftRight: Piece =
      if (highX == 6) this else Piece( rows.map( _.shiftRight ), lowX + 1, highX + 1 )

    def height: Int = rows.length
  }

  object Piece {

    val BarH: Piece = Piece.init( "####" )
    val Cross: Piece = Piece.init(
      ".#.",
      "###",
      ".#."
    )
    val Ell: Piece = Piece.init(
      "..#",
      "..#",
      "###"
    )
    val BarV: Piece = Piece.init( "#", "#", "#", "#" )
    val Square: Piece = Piece.init(
      "##",
      "##"
    )

    val basicOrder: Vector[Piece] = Vector( BarH, Cross, Ell, BarV, Square )

    @tailrec
    private def shiftLeft( piece: Piece, n: Int ): Piece =
      if (n == 0) piece
      else {
        val shiftedOnce: Piece = piece.shiftLeft
        if (n == 1 || (shiftedOnce eq piece))
          shiftedOnce
        else
          shiftLeft( shiftedOnce, n - 1 )
      }

    @tailrec
    private def shiftRight( piece: Piece, n: Int ): Piece =
      if (n == 0) piece
      else {
        val shiftedOnce: Piece = piece.shiftRight
        if (n == 1 || (shiftedOnce eq piece))
          shiftedOnce
        else
          shiftRight( shiftedOnce, n - 1 )
      }

    def init( row: String, rows: String* ): Piece = {
      val rows0: NonEmptyList[Row] = NonEmptyList( row, rows.toList ).reverse.map( Row( _ ) )
      val lowSetBit                = rows0.toList.mapFilter( _.lowSetBit ).minOption.getOrElse( 0 )
      val highSetBit               = rows0.toList.mapFilter( _.highSetBit ).maxOption.getOrElse( 0 )

      val piece0 = Piece( rows0, lowSetBit, highSetBit )

      if (lowSetBit == 2)
        piece0
      else if (lowSetBit < 2)
        shiftRight( piece0, 2 - lowSetBit )
      else
        shiftLeft( piece0, lowSetBit - 2 )
    }

  }

  /**
    *
    * @param piece the falling piece
    * @param pieceHeight height of the bottom row
    */
  case class Grid(
      levels: Vector[Row],
      piece: Piece,
      pieceHeight: Int,
      pieceMoved: Int,
      jetOrder: Vector[Jet],
      jetCount: Int,
      pieceOrder: Vector[Piece],
      pieceCount: Long
  ) {
    private def canPlace( piece: Piece, height: Int ): Boolean = {
      piece.rows.toList.zipWithIndex.forall {
        case ( row, i ) =>
          val rowHeight = height + i

          !levels.lift( rowHeight ).exists( gridRow => row.intersect( gridRow ) )
      }
    }

    def mergeWithLevels( piece: Piece, height: Int ): Vector[Row] = {
      val mergedRows: Vector[Row] =
        piece.rows.toList.toVector.zipWithIndex
          .map {
            case ( pieceRow, i ) =>
              levels.lift( height + i ) match {
                case None        => pieceRow
                case Some( row ) => pieceRow.merge( row )
              }
          }

      levels.patch( height, mergedRows, piece.height )
    }

    def goDown: Grid = {
      if (pieceHeight > 0 && canPlace( piece, pieceHeight - 1 )) {
        copy(
          pieceHeight = pieceHeight - 1,
          pieceMoved = pieceMoved + 1
        )
      } else {
        val mergedLevels: Vector[Row] = mergeWithLevels( piece, pieceHeight )
        val newPieceHeight            = mergedLevels.size + 3
        copy(
          levels = mergedLevels,
          piece = pieceOrder( (pieceCount % pieceOrder.size).toInt ),
          pieceHeight = newPieceHeight,
          pieceMoved = 0,
          pieceCount = pieceCount + 1
        )

      }

    }

    def applyJet: Grid = {
      val jet: Jet = jetOrder( jetCount % jetOrder.size )

      val shiftedPiece = jet match {
        case Jet.Left  => piece.shiftLeft
        case Jet.Right => piece.shiftRight
      }

      val nextPiece: Piece =
        if (canPlace( shiftedPiece, pieceHeight ))
          shiftedPiece
        else
          piece

      copy( piece = nextPiece, jetCount = jetCount + 1 )
    }

    override def toString: String = {
      val levelRepr: Vector[String] =
        levels
          .map( _.repr( '#' ) )
          .padTo( pieceHeight + piece.height, "......." )

      val rowsRepr = piece.rows.zipWithIndex.foldLeft( levelRepr ) {
        case ( acc, ( r, i ) ) =>
          val pieceRowRepr: String = r.repr( '@' )
          val current: String      = acc( pieceHeight + i )
          val merged: String       = (current.zip( pieceRowRepr ) ).map { case ( c, p ) => if (c == '.') p else c }.mkString
          acc.updated( pieceHeight + i, merged )
      }

      (rowsRepr.reverse.map( r => s"|$r|" ) :+ "+-------+").mkString( "\n" ) + "\n."
    }

  }

  object Grid {
    def init( pieceOrder: Vector[Piece], jetOrder: Vector[Jet] ): Grid = {
      val firstPiece = pieceOrder( 0 )
      Grid(
        levels = Vector.empty,
        piece = firstPiece,
        pieceHeight = 3,
        pieceMoved = 0,
        jetOrder = jetOrder,
        jetCount = 0,
        pieceOrder = pieceOrder,
        pieceCount = 1
      )
    }
  }

  abstract class GridRunner {

    def inspect( grid: Grid ): Either[Long, ( Grid, GridRunner )]

    def step( grid: Grid, nextIsFall: Boolean ): Either[Long, ( Grid, GridRunner )] =
      if (nextIsFall)
        inspect( grid.goDown )
      else
        Right( ( grid.applyJet, this ) )

    def run( initGrid: Grid ): Long = GridRunner.loop( this, initGrid, nextIsFall = false )

  }

  object GridRunner {

    @tailrec
    private def loop( runner: GridRunner, grid: Grid, nextIsFall: Boolean ): Long =
      runner.step( grid, nextIsFall ) match {
        case Left( a )                       => a
        case Right( ( newGrid, newRunner ) ) => loop( newRunner, newGrid, !nextIsFall )
      }

  }

  case class BasicGridRunner( targetPieceCount: Long ) extends GridRunner {
    override def inspect( grid: Grid ): Either[Long, ( Grid, GridRunner )] =
      if (grid.pieceCount > targetPieceCount)
        Left( grid.levels.size.toLong )
      else
        Right( ( grid, this ) )
  }

  case class GridSummary(
      pieceCount: Long,
      pieceMoved: Int,
      gridHeight: Int,
      top: Vector[Option[Int]]
  ) {
    def repeats( other: GridSummary, piecesPeriod: Int ): Option[( Long, Int )] = {
      val piecesSince: Long = pieceCount - other.pieceCount
      Option.when( top == other.top && pieceMoved == other.pieceMoved && piecesSince % piecesPeriod == 0 )(
        ( piecesSince, gridHeight - other.gridHeight )
      )
    }

  }

  object GridSummary {
    def apply( grid: Grid ): GridSummary = {

      GridSummary(
        grid.pieceCount,
        grid.pieceMoved,
        grid.levels.size,
        (0 to 6)
          .map(
            x =>
              grid.levels.indices
                .find( y => grid.levels( grid.levels.size - y - 1 ).getBit( x ) )
          )
          .toVector
      )
    }
  }

  case class PeriodSearchGridRunner( targetPieceCount: Long, summaries: List[GridSummary] ) extends GridRunner {
    override def inspect( grid: Grid ): Either[Long, ( Grid, GridRunner )] = {
      val piecesPeriod: Int = grid.pieceOrder.size

      if (grid.jetCount % grid.jetOrder.size != 0) Right( ( grid, this ) )
      else {
        val nextSummary: GridSummary = GridSummary( grid )

        summaries.collectFirstSome( nextSummary.repeats( _, piecesPeriod ) ) match {
          case None => Right( ( grid, copy( summaries = nextSummary :: summaries ) ) )
          case Some( ( period, height ) ) =>
            println( s"$nextSummary is a repeat position after $period pieces with $height more height" )
            Right( ( grid, PeriodicGridRunner( targetPieceCount, period, height ) ) )
        }
      }

    }
  }

  case class PeriodicGridRunner( targetPieceCount: Long, period: Long, heightPerPeriod: Int ) extends GridRunner {
    override def inspect( grid: Grid ): Either[Long, ( Grid, GridRunner )] = {
      val i: Long = targetPieceCount - grid.pieceCount + 1L
      val r: Long = i % period
      if (r != 0) Right( ( grid, this ) )
      else {
        val q: Long = i / period
        Left( grid.levels.size.toLong + q * heightPerPeriod.toLong )
      }
    }

  }

}

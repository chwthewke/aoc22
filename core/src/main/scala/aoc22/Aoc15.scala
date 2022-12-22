package aoc22

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.derived.semiauto
import cats.effect.Sync
import cats.kernel.CommutativeGroup
import cats.parse.Parser
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

class Aoc15[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc15._

  val upperBound: Int = if (live) 4000000 else 20

  val diagBounds: Interval = Interval( 0, 2 * upperBound )

  val sampleRow: Int = if (live) 2000000 else 10

  def getObservations: F[Vector[Observation]] =
    lines.through( parseLines( parsers.observation ) ).compile.toVector

  def rowCoverage( observations: Vector[Observation], y: Int ): Int = {
    val observed: Intervals =
      Intervals.of( observations.mapFilter( _.rowCoverage( y ) ) )

    val beacons: Vector[Position] = observations
      .map( _.beacon )
      .filter( b => b.y == y && observed.contains( b.x ) )
      .distinct

    observed.size - beacons.size
  }

  override def basic: F[String] =
    getObservations.map( rowCoverage( _, sampleRow ).toString )

  // We observe that diagonal coverage is monotonous within the partially covered diagonals
  // of one or more sensors.
  // In other words, if diagonals D1: (x+y=a1), D2: (x+y=a2) are partially covered by the same
  // set of sensors, and are both fully covered, then D: (x+y=a) is also fully covered
  // for all a in [ a1; a2 ].
  // Since we expect to find only a single non-fully covered diagonal, that diagonal must be
  // at the outer edge of the partial coverage of one of the sensor. (Note that the inner edge
  // is strictly more covered than the outer edge - unless it is also the outer edge of some
  // other sensor)

  def locateBeacon( observations: Vector[Observation] ): Option[Position] = {
    def stops: SortedSet[Int] =
      observations
        .foldMap { obs =>
          val partialDiagonalCoverage = obs.partiallyCoveredDiagonals
          SortedSet( partialDiagonalCoverage.low - 1, partialDiagonalCoverage.high + 1 )
        }
        .filter( diagBounds.contains ) + diagBounds.low + diagBounds.high

    stops.collectFirstSome { d =>
      val coverage     = Intervals.of( observations.mapFilter( _.diagonalCoverage( d ) ) )
      val fullCoverage = diagonalXBounds( d )
      if (coverage.includes( fullCoverage )) None
      else {
        // println( s"@ x+y=$d $coverage" )
        coverage.intervals
          .maxBefore( Interval( fullCoverage.high + 1, fullCoverage.high + 1 ) )
          .map( _.low - 1 ) // that's our x
          .map( x => Position( x, d - x ) )
      }
    }
  }

  // set of x s.t. (x, d - x) is in-bounds,
  // i.e. 0 <= x <= upperBound and 0 <= d - x <= upperBound
  // (the last is <=> d >= x >= d - upperBound)
  // We also assume 0 <= d <= 2 * upperBound
  private def diagonalXBounds( d: Int ): Interval =
    Interval( 0 max (d - upperBound), upperBound min d )

  override def bonus: F[String] =
    getObservations
      .map( locateBeacon )
      .flatMap( _.toRight( "No beacon found" ).into[F] )
      .map( pos => (4000000L * pos.x.toLong + pos.y.toLong).toString )

}

object Aoc15 {
  case class Position( x: Int, y: Int ) {
    // L¹ this time
    def norm: Int = x.abs + y.abs
  }

  object Position {
    implicit val positionGroup: CommutativeGroup[Position] = new CommutativeGroup[Position] {
      override def inverse( a: Position ): Position = Position( -a.x, -a.y )

      override def empty: Position = Position( 0, 0 )

      override def combine( a: Position, b: Position ): Position = Position( a.x + b.x, a.y + b.y )
    }
  }

  case class Observation( sensor: Position, beacon: Position ) {
    override def toString: String =
      s"(${sensor.x}, ${sensor.y}) -> (${beacon.x}, ${beacon.y})"

    val dist: Int = beacon.remove( sensor ).norm

    def rowCoverage( y: Int ): Option[Interval] = {
      val dx: Int = dist - (sensor.y - y).abs

      Option.when( dx >= 0 )( Interval( sensor.x - dx, sensor.x + dx ) )
    }

    // values of d s.t. D = (x + y = d) is partially within dist of the sensor
    // i.e. abs( sensor.y - D(sensor.x) ) <= dist
    // i.e. abs( d - (sensor.x + sensor.y) ) <= dist
    val partiallyCoveredDiagonals: Interval = {
      val n: Int = sensor.x + sensor.y
      Interval( n - dist, n + dist )
    }

    // values of x' s.t D: (x + y = d) is within dist of the sensor at x = x'
    // i.e. (x', d - x') is within dist of the sensor
    // after some calculations, this yields
    // K - dist <= 2x <= K + dist
    // with K = sensor.x - sensor.y + d
    def diagonalCoverage( d: Int ): Option[Interval] = Option.when( partiallyCoveredDiagonals.contains( d ) ) {
      val k = sensor.x - sensor.y + d
      Interval( (k - dist + 1) / 2, (k + dist) / 2 )
    }
  }

  object parsers {
    import cats.parse.Numbers.signedIntString

    private val coordinate: Parser[Int] = signedIntString.mapFilter( _.toIntOption )

    private val position: Parser[Position] =
      (
        Parser.string( "x=" ) *> coordinate,
        Parser.string( ", y=" ) *> coordinate
      ).mapN( Position( _, _ ) )

    val observation: Parser[Observation] =
      (
        Parser.string( "Sensor at " ) *> position,
        Parser.string( ": closest beacon is at " ) *> position
      ).mapN( Observation )
  }

  // bounds-inclusive range with low <= high
  case class Interval( low: Int, high: Int ) {
    def includes( other: Interval ): Boolean =
      low <= other.low && high >= other.high

    def overlaps( other: Interval ): Boolean =
      !(low > other.high || high < other.low)

    def touches( other: Interval ): Boolean =
      !(low > other.high + 1 || other.low > high + 1)

    def contains( x: Int ): Boolean =
      high >= x && low <= x

    val size: Int = high - low + 1

    override def toString: String = s"[ $low; $high ]"
  }

  object Interval {
    def of( a: Int, b: Int ): Interval = Interval( a min b, a max b )

    implicit val intervalOrder: Order[Interval] = semiauto.order[Interval]
  }

  // non-overlapping interval collection
  case class Intervals( intervals: SortedSet[Interval] ) extends AnyVal {
    def add( interval: Interval ): Intervals =
      Intervals( Intervals.addLoop( intervals, interval ) )

    final private[Intervals] def maxBefore( x: Int ): Option[Interval] =
      intervals.maxBefore( Interval( x, x ) )

    def contains( x: Int ): Boolean =
      maxBefore( x + 1 ).exists( _.contains( x ) )

    def includes( interval: Interval ): Boolean =
      maxBefore( interval.low + 1 ).exists( _.includes( interval ) )

    def size: Int = intervals.foldMap( _.size )

    def min: Option[Int] = intervals.headOption.map( _.low )

    def max: Option[Int] = intervals.lastOption.map( _.high )

    override def toString: String = intervals.mkString( "(", ", ", ")" )
  }

  object Intervals {
    def of( intervals: Iterable[Interval] ): Intervals =
      intervals.foldLeft( Intervals( SortedSet.empty ) )( _.add( _ ) )

    // assumes i1 and i2 touch, i.e. i1.low <= i2.high + 1 && i2.low <= i1.high + 1
    private def touchingUnion( i1: Interval, i2: Interval ): Interval =
      Interval( i1.low min i2.low, i1.high max i2.high )

    @tailrec
    private[Intervals] def addLoop( intervals: SortedSet[Interval], interval: Interval ): SortedSet[Interval] = {
      // find an interval touching the argument
      // 1. it must have x.low <= interval.high + 1, i.e. x < Interval( interval.high + 2, interval.high + 2 )
      // 2. among these, if any touches then the largest (i.e. the one w/ largest low) does
      val ub: Int = interval.high + 2
      intervals
        .maxBefore( Interval( ub, ub ) )
        .filter( interval.touches ) match {
        case None =>
          // if none, add the argument to the intervals
          intervals + interval
        case Some( overlapping ) =>
          // if one, remove it and recursively add the union of the argument with it
          if (overlapping.includes( interval ))
            // slight optimization instead of calling addLoop( intervals - overlapping, overlapping ) in this case
            intervals
          else
            addLoop( intervals - overlapping, touchingUnion( interval, overlapping ) )
      }
    }
  }

}

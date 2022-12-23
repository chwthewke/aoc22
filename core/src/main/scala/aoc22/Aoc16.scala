package aoc22

import cats.Parallel
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.effect.Sync
import cats.parse.Parser
import cats.parse.Parser0
import cats.syntax.all._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

class Aoc16[F[_]: Sync: Parallel]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc16._

  def getRooms: F[NonEmptyVector[Room]] =
    lines
      .through( parseLines( parsers.room ) )
      .compile
      .toVector
      .flatMap( v => NonEmptyVector.fromVector( v ).toRight( "No rooms" ).into[F] )

  val start: String = "AA"

  def initRooms( timer: Int ): F[Rooms] =
    for {
      roomList <- getRooms
      paths    <- RoomsStaging( "AA", roomList ).paths[F]
    } yield Rooms( "AA", timer, roomList, paths )

  override def basic: F[String] =
    initRooms( 30 ).map( _.bestSteps.score.toString )

  override def bonus: F[String] =
    initRooms( 26 )
      .flatMap { rooms =>
//        println( rooms.paths )

        Sync[F].blocking {
          val stepsWithElephant = rooms.bestStepsWithElephant
          //            println( stepsWithElephant )
          stepsWithElephant.score.toString
        }
      }
}

object Aoc16 {

  case class Room( name: String, flow: Int, neighbours: NonEmptyVector[String] ) {
    override def toString: String =
      s"$name($flow)".padTo( 8, ' ' ) + s" -> ${neighbours.mkString_( ", " )}"
  }

  object Room {
    def apply( name: String, flow: Int, neighbours: NonEmptyList[String] ): Room =
      Room( name, flow, neighbours.toNev )

    implicit val roomShow: Show[Room] = Show.fromToString[Room]
  }

  case class Path( from: String, to: String, length: Int, flowAtEnd: Int, steps: List[String] ) {
    def append( room: String, flow: Int ): Path =
      Path( from, room, length + 1, flow, room :: steps )
  }

  case class Paths( byOrigin: Map[String, Map[String, Path]] )

  case class RoomsStaging( init: String, rooms: NonEmptyVector[Room] ) {
    private val roomsMap: Map[String, Room] =
      rooms.map( r => ( r.name, r ) ).toVector.toMap

    private val usefulRooms: Set[String] =
      rooms.filter( _.flow > 0 ).map( _.name ).toSet

    def paths[F[_]: Sync: Parallel]: F[Paths] =
      (usefulRooms + init).toVector
        .parTraverse( from => Sync[F].blocking( pathsFrom( from ) ).tupleLeft( from ) )
        .map( byOrig => Paths( byOrig.toMap ) )

    def pathsFrom( room: String ): Map[String, Path] =
      pathsLoop(
        Map( room -> Path( room, room, 0, roomsMap( room ).flow, Nil ) ),
        Vector( room ),
        usefulRooms - room,
        usefulRooms - room
      )

    @tailrec
    private def pathsLoop(
        seen: Map[String, Path],
        open: Vector[String],
        goal: Set[String],
        initGoal: Set[String]
    ): Map[String, Path] =
      if (open.isEmpty || goal.isEmpty) {
        seen.filter { case ( d, _ ) => initGoal( d ) }
      } else {
        val look     = open.head
        val lookRoom = roomsMap( look )
        val lookPath = seen( look )

        val next =
          lookRoom.neighbours.filterNot( seen.contains )

        val nextPaths =
          next.fproduct( n => lookPath.append( n, roomsMap( n ).flow ) )
        val newSeen = seen ++ nextPaths
        val newOpen = open.tail ++ next
        val newGoal = next.foldLeft( goal )( _ - _ )

        pathsLoop( newSeen, newOpen, newGoal, initGoal )
      }

  }

  case class Steps(
      length: Int,
      score: Int,
      pos: String,
      toOpen: Set[String]
  )

  object StepsE {
    private val id: AtomicInteger = new AtomicInteger( 0 )
    def nextId: Int               = id.incrementAndGet()

    implicit val stepsEOrdering: Ordering[StepsE] =
      ( x: StepsE, y: StepsE ) => x.compareTo( y )
  }

  case class StepsE( // StepsWithElephant
      ourLength: Int,
      ourPos: String,
      elephantLength: Int,
      elephantPos: String,
      score: Int,
      toOpen: Set[String],
      // debug
      prev: Option[StepsE]
  ) {
    private val id: Int = StepsE.nextId

    def compareTo( other: StepsE ): Int = {
      val scoreC = other.score.compare( score )
      if (scoreC != 0)
        scoreC
      else {
        val lengthC = (ourLength + elephantLength).compare( other.ourLength + other.elephantLength )
        if (lengthC != 0) lengthC
        else id.compare( other.id )
      }
    }

    override def toString: String = toString0( "", "" )

    @tailrec
    private def toString0( indent: String, acc: String ): String = {
      val selfTxt = s"${indent}StepsE( ourLength=$ourLength, ourPos=$ourPos, " +
        s"elephantLength=$elephantLength, elephantPos=$elephantPos, " +
        s"score=$score, toOpen=${toOpen.mkString( "(", ", ", ")" )} )"

      val nextAcc = s"$acc$selfTxt\n"

      prev match {
        case None          => nextAcc
        case Some( steps ) => steps.toString0( "  " + indent, nextAcc )
      }
    }
  }

  case class Rooms( init: String, timer: Int, rooms: NonEmptyVector[Room], paths: Paths ) {

    val flows: Map[String, Int] = rooms.map( r => ( r.name, r.flow ) ).toVector.toMap

    override def toString: String = {
      val roomsTxt =
        rooms
          .map( r => (if (init == r.name) "*" else " ") + r.toString )
          .mkString_( "\n" )

      val pathsTxt =
        paths.byOrigin
          .map {
            case ( orig, paths1 ) =>
              paths1
                .map { case ( dest, Path( _, _, l, _, _ ) ) => s"$dest($l)" }
                .mkString( s"$orig:", ", ", "" )
          }
          .mkString( "\n" )

      s"""ROOMS
         |$roomsTxt
         |
         |PATHS
         |$pathsTxt
         |""".stripMargin
    }

    val valvesToOpen: Set[String] = rooms.filter( _.flow > 0 ).map( _.name ).toSet

    val initSteps: Steps =
      Steps( 0, 0, init, valvesToOpen )

    @tailrec
    final def floodFillLoop( seen: Vector[Steps], best: Steps ): Steps =
      if (seen.isEmpty)
        best
      else {
        val look = seen.head

        val next: Set[Steps] = look.toOpen.flatMap { p =>
          val path: Path      = paths.byOrigin( look.pos )( p )
          val nextLength: Int = look.length + path.length + 1
          Option.when( nextLength <= timer )(
            Steps(
              nextLength,
              look.score + (timer - nextLength) * path.flowAtEnd,
              p,
              look.toOpen - p
            )
          )
        }

        val newSeen: Vector[Steps] = seen.tail ++ next
        val newBest: Steps         = next.foldLeft( best )( ( b, s ) => if (s.score > b.score) s else b )

        floodFillLoop( newSeen, newBest )
      }

    def bestSteps: Steps = {
      floodFillLoop( Vector( initSteps ), initSteps )
    }

    val initStepsE: StepsE =
      StepsE( 0, init, 0, init, 0, valvesToOpen, None )
// testing testing
//      StepsE( 3, "JJ", 7, "HH", 20 * 24 + 21 * 23 + 22 * 19, valvesToOpen - "JJ" - "DD" - "HH", None )
//      StepsE( 3, "JJ", 2, "DD", 20 * 24 + 21 * 23, valvesToOpen - "JJ" - "DD", None )

    def scoreUpperBound( steps: StepsE ): Int = {
      def optimisticOpenings( length: Int, pos: String ): Vector[Int] =
        (timer - length - (if (steps.toOpen.contains( pos )) 1 else 2)).to( 0 ).by( -2 ).toVector

      val ts: Vector[Int] =
        (optimisticOpenings( steps.ourLength, steps.ourPos ) ++
          optimisticOpenings( steps.elephantLength, steps.elephantPos )).sorted.reverse

      val valves: Vector[Int] = steps.toOpen.map( flows( _ ) ).toVector.sorted.reverse

      steps.score + valves.zip( ts ).map { case ( f, t ) => f * t }.sum
    }

    @tailrec
    final def floodFillLoopWithElephant( seen: SortedSet[StepsE], best: StepsE ): StepsE =
      if (seen.isEmpty)
        best
      else {
        val look = seen.head

        def nextSingle( currLength: Int, currPos: String )( append: ( Path, Int ) => StepsE ): Set[StepsE] = {
//          println( look.toOpen )

          look.toOpen.flatMap { p =>
            val path: Path      = paths.byOrigin( currPos )( p )
            val nextLength: Int = currLength + path.length + 1

            Option.when( nextLength <= timer )( append( path, nextLength ) )
          }
        }

        def ourMoves: Set[StepsE] =
          nextSingle( look.ourLength, look.ourPos )(
            ( path, nextLength ) =>
              StepsE(
                nextLength,
                path.to,
                look.elephantLength,
                look.elephantPos,
                look.score + (timer - nextLength) * path.flowAtEnd,
                look.toOpen - path.to,
                look.some
              )
          )

        def elephantMoves: Set[StepsE] =
          nextSingle( look.elephantLength, look.elephantPos )(
            ( path, nextLength ) =>
              StepsE(
                look.ourLength,
                look.ourPos,
                nextLength,
                path.to,
                look.score + (timer - nextLength) * path.flowAtEnd,
                look.toOpen - path.to,
                look.some
              )
          )

        val next: Iterable[StepsE] = {
          if (look.toOpen.isEmpty) {
            Nil
          } else if (look.toOpen.size == 1) {
//            println( s"$look EITHER MOVE" )
            ourMoves ++ elephantMoves
          } else if (look.ourLength < look.elephantLength) {
//            println( s"$look OUR MOVE" )
            ourMoves
          } else if (look.elephantLength < look.ourLength) {
//            println( s"$look E'S MOVE" )
            val z = elephantMoves
//            println( "+ELE+" )
//            println( z )
//            println( "-ELE-" )
            z
          } else {
//            println( s"$look BOTH MOVE" )
            val currLength: Int = look.ourLength // same as look.elephantLength, for clarity

            // if we start from the same point (should be 1st move only),
            // do not consider swapped destinations
            val destCompat: ( String, String ) => Boolean =
              if (look.ourPos == look.elephantPos)( x: String, y: String ) => x < y
              else
                ( x: String, y: String ) => x != y

            // TODO not sure if starting with toOpen or paths is better
            for {
              ourDest      <- look.toOpen
              elephantDest <- look.toOpen if destCompat( elephantDest, ourDest )
              ourPath      <- paths.byOrigin( look.ourPos ).get( ourDest ) if ourPath.length + currLength < timer
              elephantPath <- paths.byOrigin( look.elephantPos ).get( elephantDest )
              if elephantPath.length + currLength < timer
            } yield {
              val ourNextLength      = currLength + ourPath.length + 1
              val elephantNextLength = currLength + elephantPath.length + 1

              StepsE(
                ourNextLength,
                ourDest,
                elephantNextLength,
                elephantDest,
                look.score +
                  (timer - ourNextLength) * ourPath.flowAtEnd +
                  (timer - elephantNextLength) * elephantPath.flowAtEnd,
                look.toOpen - ourDest - elephantDest,
                look.some
              )
            }
          }
        }

        val newBest: StepsE            = next.foldLeft( best )( ( b, s ) => if (s.score > b.score) s else b )
        val newSeen: SortedSet[StepsE] = seen.tail ++ next.filter( s => scoreUpperBound( s ) > newBest.score )

        floodFillLoopWithElephant( newSeen, newBest )
      }

    def bestStepsWithElephant: StepsE =
      floodFillLoopWithElephant( SortedSet( initStepsE ), initStepsE )

  }

  object Rooms {
    implicit val roomsShow: Show[Rooms] = Show.fromToString[Rooms]
  }

  object parsers {
    import cats.parse.Numbers.nonNegativeIntString
    import cats.parse.Rfc5234.alpha

    private val valveName: Parser[String] = Parser.string( alpha.rep )

    private val pressure: Parser[Int] = nonNegativeIntString.mapFilter( _.toIntOption )

    private val optS: Parser0[Unit] = Parser.char( 's' ).?.void
    private val tunnels: Parser[NonEmptyList[String]] =
      Parser.string( "tunnel" ).void *> optS *>
        Parser.string( " lead" ).void *> optS *>
        Parser.string( " to valve" ).void *> optS *>
        Parser.string( " " ).void *> valveName.repSep( Parser.string( ", " ).void )

    val room: Parser[Room] =
      (
        Parser.string( "Valve " ) *> valveName,
        Parser.string( " has flow rate=" ) *> pressure,
        Parser.string( "; " ) *> tunnels
      ).mapN( Room( _, _, _ ) )
  }
}

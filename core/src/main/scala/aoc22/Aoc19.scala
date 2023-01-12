package aoc22

import cats.PartialOrder
import cats.data.NonEmptyVector
import cats.effect.Sync
import cats.kernel.Group
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

class Aoc19[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc19._

  override def basic: F[String] =
    lines
      .through( parseLines( parsers.blueprint ) )
      .compile
      .toVector
      .flatMap(
        bs =>
          Sync[F].delay(
            bs.map( b => s"BP #${b.id}: ${naiveOpt( b )}" ).mkString( "\n" )
          )
      )

  override def bonus: F[String] = ???
}

object Aoc19 {

  case class Counts( ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0 )

  object Counts {

    private val fields: List[Counts => Int] =
      List( _.ore, _.clay, _.obsidian, _.geode )

    def partialCompare( left: Counts, right: Counts ) = {

      @tailrec
      def partialCompareLoop(
          expected: Int,
          fields: List[Counts => Int]
      ): Double =
        fields match {
          case Nil => expected.toDouble
          case field :: more =>
            val fieldCmp = field( left ).compareTo( field( right ) )
            if (expected == 0 || fieldCmp == expected) partialCompareLoop( fieldCmp, more )
            else if (fieldCmp == 0) partialCompareLoop( expected, more )
            else Double.NaN
        }

      partialCompareLoop( 0, fields )
    }

    implicit val countsPartialOrder: PartialOrder[Counts] = PartialOrder.from( partialCompare )

    val empty: Counts = Counts()

    implicit val countsGroup: Group[Counts] = new Group[Counts] {
      override def inverse( a: Counts ): Counts = Counts( -a.ore, -a.clay, -a.obsidian, -a.geode )

      override def empty: Counts = Counts.empty

      override def combine( x: Counts, y: Counts ): Counts =
        Counts( x.ore + y.ore, x.clay + y.clay, x.obsidian + y.obsidian, x.geode + y.geode )
    }
  }

  case class Inventory(
      resources: Counts,
      robots: Counts
  ) {
    def moves( blueprint: Blueprint ): NonEmptyVector[( Option[Item], Counts )] =
      NonEmptyVector(
        ( None, Counts.empty ),
        Item.values.mapFilter { item =>
          val cost = blueprint.costOf( item )
          Option.when( cost <= resources )( ( Some( item ), cost ) )
        }
      )

    def next( orderOpt: Option[Item], cost: Counts ): Inventory =
      Inventory(
        resources.remove( cost ) |+| robots,
        orderOpt.foldLeft( robots )( ( r, i ) => r |+| i.counts )
      )

    def nextMoves( blueprint: Blueprint ): NonEmptyVector[Inventory] =
      moves( blueprint ).map( (next _).tupled )
  }

  object Inventory {
    val init: Inventory = Inventory( Counts.empty, Counts( ore = 1 ) )

    def partialCompare( left: Inventory, right: Inventory ): Double = {
      val resourcesCmp = Counts.partialCompare( left.resources, right.resources )
      if (resourcesCmp.isNaN) Double.NaN
      else {
        val robotsCmp = Counts.partialCompare( left.robots, right.robots )
        if (robotsCmp == 0 || resourcesCmp == robotsCmp) resourcesCmp
        else if (resourcesCmp == 0) robotsCmp
        else Double.NaN
      }
    }

    implicit val inventoryPartialOrder: PartialOrder[Inventory] = PartialOrder.from( partialCompare )
  }

  case class Inventories( inventories: Vector[Inventory] ) extends AnyVal {
    def length: Int = inventories.size

    def add( candidate: Inventory ): Inventories = {
      @tailrec
      def addLoop( ix: Int ): Vector[Inventory] =
        if (ix >= inventories.length) inventories :+ candidate
        else {
          val cmp = Inventory.partialCompare( candidate, inventories( ix ) )
          if (cmp <= 0) inventories
          else if (cmp > 0)
            inventories.take( ix ) ++ inventories.drop( ix + 1 ).filterNot( _ <= candidate ) :+ candidate
          else
            addLoop( ix + 1 )
        }

      Inventories( addLoop( 0 ) )
    }
  }

  object Inventories {
    val empty: Inventories = Inventories( Vector.empty )
    val init: Inventories  = Inventories( Vector( Inventory.init ) )
  }

  val MaxTurn: Int = 24

  def naiveOpt( blueprint: Blueprint ): Int = {
    @tailrec
    def loop( turn: Int, states: Inventories ): Int = {
      println( s"Turn ${turn + 1} from ${states.length} states" )
      if (turn == MaxTurn - 1) // nothing we do on the last turn changes anything
        states.inventories.map( _.next( None, Counts.empty ) ).map( _.resources.geode ).max
      else
        loop(
          turn + 1,
          states.inventories.foldLeft( Inventories.empty )(
            ( invs, state ) => state.nextMoves( blueprint ).foldLeft( invs )( _.add( _ ) )
          )
        )
    }

    loop( 0, Inventories.init )
  }

  sealed abstract class Item( val counts: Counts ) extends EnumEntry
  object Item extends Enum[Item] {
    case object Ore      extends Item( Counts( ore = 1 ) )
    case object Clay     extends Item( Counts( clay = 1 ) )
    case object Obsidian extends Item( Counts( obsidian = 1 ) )
    case object Geode    extends Item( Counts( geode = 1 ) )

    override val values: Vector[Item] = findValues.toVector
  }

  case class Blueprint(
      id: Int,
      orePerOreRobot: Int,
      orePerClayRobot: Int,
      orePerObsidianRobot: Int,
      clayPerObsidianRobot: Int,
      orePerGeodeRobot: Int,
      obsidianPerGeodeRobot: Int
  ) {
    def costOf( item: Item ): Counts = item match {
      case Item.Ore      => Counts( ore = orePerOreRobot )
      case Item.Clay     => Counts( ore = orePerClayRobot )
      case Item.Obsidian => Counts( ore = orePerObsidianRobot, clay = clayPerObsidianRobot )
      case Item.Geode    => Counts( ore = orePerGeodeRobot, obsidian = obsidianPerGeodeRobot )
    }
  }

  object parsers {
    import cats.parse.Numbers.nonNegativeIntString

    val posInt: Parser[Int] = nonNegativeIntString.mapFilter( _.toIntOption )

    val blueprint: Parser[Blueprint] =
      (
        Parser.string( "Blueprint " ) *> posInt,
        Parser.string( ": Each ore robot costs " ) *> posInt,
        Parser.string( " ore. Each clay robot costs " ) *> posInt,
        Parser.string( " ore. Each obsidian robot costs " ) *> posInt,
        Parser.string( " ore and " ) *> posInt,
        Parser.string( " clay. Each geode robot costs " ) *> posInt,
        Parser.string( " ore and " ) *> posInt <* Parser.string( " obsidian." )
      ).mapN( Blueprint )
  }
}

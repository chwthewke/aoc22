package aoc22

import cats.Monoid
import cats.PartialOrder
import cats.derived.semiauto
import cats.effect.Sync
import cats.kernel.Group
import cats.parse.Parser
import cats.syntax.all._
import enumeratum.Enum
import enumeratum.EnumEntry

class Aoc19[F[_]: Sync]( srcFile: String, live: Boolean ) extends Day.Of[F]( srcFile, live ) {
  import Aoc19._

  override def basic: F[String] =
    lines
      .through( parseLines( parsers.blueprint ) )
      .compile
      .toVector
      .map( bs => s"parsed ${bs.size} blueprints" )
  override def bonus: F[String] = ???
}

object Aoc19 {

  case class Counts( ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0 ) {}

  object Counts {
    @inline
    private def compare( nextComparison: Int, rest: Int => Double )( expectedComparison: Int ): Double =
      if (expectedComparison * nextComparison == -1) Double.NaN
      else rest( if (expectedComparison == 0) nextComparison else expectedComparison )

    implicit val countsPartialOrder: PartialOrder[Counts] = PartialOrder.from { ( c1, c2 ) =>
      compare(
        c1.clay.compareTo( c2.clay ),
        compare(
          c1.obsidian.compareTo( c2.obsidian ),
          compare(
            c1.geode.compareTo( c2.geode ),
            _.toDouble
          )
        )
      )( c1.ore.compareTo( c2.ore ) )
    }

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
    def next( blueprint: Blueprint, orderOpt: Option[Item] ): Option[Inventory] = orderOpt match {
      case Some( order ) =>
        val orderCost = blueprint.costOf( order )

        Option.when( orderCost <= resources )(
          Inventory(
            resources.remove( orderCost ) |+| robots,
            robots |+| order.counts
          )
        )
      case None =>
        Inventory(
          resources |+| robots,
          robots
        ).some
    }

  }

  object Inventory {
    val init: Inventory = Inventory(
      Counts.empty,
      Counts( ore = 1 )
    )
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
      case Item.Ore      => Counts( orePerOreRobot, 0, 0, 0 )
      case Item.Clay     => Counts( orePerClayRobot, 0, 0, 0 )
      case Item.Obsidian => Counts( orePerObsidianRobot, clayPerObsidianRobot, 0, 0 )
      case Item.Geode    => Counts( orePerGeodeRobot, 0, obsidianPerGeodeRobot, 0 )
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

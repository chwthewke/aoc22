package aoc22

import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.all._

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

  case class Blueprint(
      id: Int,
      orePerOreRobot: Int,
      orePerClayRobot: Int,
      orePerObsidianRobot: Int,
      clayPerObsidianRobot: Int,
      orePerGeodeRobot: Int,
      obsidianPerGeodeRobot: Int
  )

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

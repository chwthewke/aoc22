package aoc22

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import fs2.Chunk

class Aoc13[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc13._

  private def parsePacket( chunk: Chunk[String], ix: Int ): Either[String, Packet] =
    chunk.toVector
      .lift( ix )
      .toRight( "Non-full chunk" )
      .flatMap( line => parsers.packet.parseAll( line ).leftMap( formatError( line, _ ) ) )

  def getPacketPairs: F[Vector[( Packet, Packet )]] =
    lines
      .chunkN( 2, allowFewer = true )
      .evalMap(
        lines =>
          (
            parsePacket( lines, 0 ),
            parsePacket( lines, 1 )
          ).tupled.into[F]
      )
      .compile
      .toVector

  def orderedPackets( packets: Vector[( Packet, Packet )] ): Int =
    packets.zipWithIndex.foldMap {
      case ( ( p1, p2 ), ix ) =>
        if (comparePackets( p1, p2 ) <= 0) ix + 1 else 0
    }

  override def basic: F[String] =
    getPacketPairs.map( orderedPackets( _ ).toString )

  def getPackets: F[Vector[Packet]] =
    lines
      .evalMap( line => parsers.packet.parseAll( line ).leftMap( formatError( line, _ ) ).into[F] )
      .compile
      .toVector

  def getPacketsSorted: F[Vector[Packet]] =
    getPackets.map( packets => (packets ++ Vector( Packet.divider1, Packet.divider2 )).sorted )

  override def bonus: F[String] =
    getPacketsSorted
      .map(
        allPackets =>
          (1 + allPackets.indexWhere( _ eq Packet.divider1 )) *
            (1 + allPackets.indexWhere( _ eq Packet.divider2 ))
      )
      .map( _.toString )
}

object Aoc13 {
  sealed abstract class Packet extends Product
  case class IntPacket( value: Int ) extends Packet {
    override def toString: String = value.toString
  }
  case class ListPacket( packets: List[Packet] ) extends Packet {
    override def toString: String = packets.mkString( "[", ", ", "]" )
  }

  object Packet {
    implicit val packetOrder: Order[Packet] = Order.from( comparePackets )

    val divider1: Packet = ListPacket( ListPacket( IntPacket( 2 ) :: Nil ) :: Nil )
    val divider2: Packet = ListPacket( ListPacket( IntPacket( 6 ) :: Nil ) :: Nil )
  }

  def comparePackets( left: Packet, right: Packet ): Int =
    ( left, right ) match {
      case ( IntPacket( x ), IntPacket( y ) )       => x.compare( y )
      case ( ListPacket( Nil ), ListPacket( Nil ) ) => 0
      case ( ListPacket( Nil ), ListPacket( _ ) )   => -1
      case ( ListPacket( _ ), ListPacket( Nil ) )   => 1
      case ( ListPacket( h1 :: t1 ), ListPacket( h2 :: t2 ) ) =>
        val c = comparePackets( h1, h2 )
        if (c != 0) c
        else comparePackets( ListPacket( t1 ), ListPacket( t2 ) )
      case ( l @ ListPacket( _ ), p ) => comparePackets( l, ListPacket( p :: Nil ) )
      case ( p, l @ ListPacket( _ ) ) => comparePackets( ListPacket( p :: Nil ), l )
    }

  object parsers {
    import cats.parse.Numbers.nonNegativeIntString

    val intPacket: Parser[IntPacket] =
      nonNegativeIntString.mapFilter( _.toIntOption ).map( IntPacket )

    lazy val listPacket: Parser[ListPacket] =
      (Parser.char( '[' ).void ~ packet.repSep0( Parser.char( ',' ) ) ~ Parser.char( ']' ).void)
        .map { case ( ( _, list ), _ ) => ListPacket( list ) }

    lazy val packet: Parser[Packet] = intPacket.orElse( Parser.defer( listPacket ) )
  }
}

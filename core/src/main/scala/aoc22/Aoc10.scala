package aoc22

import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.all._
import fs2.Stream

class Aoc10[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc10._

  def getInstructions: Stream[F, Instr] =
    lines.through( parseLines( parsers.instr ) )

  override def basic: F[String] =
    getInstructions
      .scan( SamplingComputer.init( 20, 40 ) )( _.run( _ ) )
      .compile
      .lastOrError
      .map( _.sampled.toString )

  override def bonus: F[String] =
    getInstructions
      .scan( DrawingComputer.init )( _.run( _ ) )
      .compile
      .lastOrError
      .map( _.screen )

}

object Aoc10 {
  sealed trait Instr
  case object Noop          extends Instr
  case class AddX( x: Int ) extends Instr

  case class Computer( nextCycle: Int, register: Int ) {
    def run( instr: Instr ): Computer = instr match {
      case Noop      => Computer( nextCycle + 1, register )
      case AddX( x ) => Computer( nextCycle + 2, register + x )
    }
  }

  object Computer {
    val init: Computer = Computer( 1, 1 )
  }

  case class SamplingComputer( computer: Computer, sampled: Int, nextSampling: Int, samplingStep: Int ) {
    def run( instr: Instr ): SamplingComputer = {
      val nextComputer = computer.run( instr )

      if (nextComputer.nextCycle > nextSampling)
        SamplingComputer(
          nextComputer,
          sampled + nextSampling * computer.register,
          nextSampling + samplingStep,
          samplingStep
        )
      else
        SamplingComputer( nextComputer, sampled, nextSampling, samplingStep )
    }
  }

  object SamplingComputer {
    def init( firstSampling: Int, samplingStep: Int ): SamplingComputer =
      SamplingComputer( Computer.init, 0, firstSampling, samplingStep )
  }

  case class DrawingComputer( computer: Computer, drawn: String ) {
    def outputPixel( cycle: Int, register: Int ): Char = {
      val hpos: Int = (cycle - 1) % 40
      if ((register - hpos).abs <= 1) '#' else '.'
    }

    def run( instr: Instr ): DrawingComputer = {
      val nextComputer: Computer = computer.run( instr )

      DrawingComputer(
        nextComputer,
        drawn +
          computer.nextCycle
            .until( nextComputer.nextCycle )
            .map( outputPixel( _, computer.register ) )
            .mkString
      )
    }

    def screen: String =
      drawn.grouped( 40 ).mkString( "\n" )
  }

  object DrawingComputer {
    val init: DrawingComputer = DrawingComputer( Computer.init, "" )
  }

  object parsers {
    import cats.parse.Rfc5234.wsp
    import cats.parse.Numbers.signedIntString

    val noop: Parser[Noop.type] = Parser.string( "noop" ).as( Noop )

    val addx: Parser[AddX] = Parser.string( "addx" ) *> wsp *> signedIntString.mapFilter( _.toIntOption ).map( AddX )

    val instr: Parser[Instr] = noop.orElse( addx )
  }

}

package aoc22

import cats.Show
import cats.data.NonEmptyVector
import cats.derived.semiauto
import cats.effect.Sync
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functorFilter._
import cats.syntax.flatMap._
import cats.syntax.option._

class Aoc5[F[_]: Sync] extends Day[F] {
  import Aoc5._

  private def readInstructions( live: Boolean ): F[Instructions] =
    Data
      .lines[F]( 5, live )
      .evalScan( ReadingStacks( Nil ): Accumulator )(
        ( acc, line ) => acc.readLine( line ).leftMap( Error( _ ) ).liftTo[F]
      )
      .compile
      .lastOrError
      .flatMap( _.validate.leftMap( Error( _ ) ).liftTo[F] )

  override def basic( live: Boolean ): F[String] =
    readInstructions( live )
      .flatMap( instr => instr.evalMoves.leftMap( Error( _ ) ).liftTo[F] )

  override def bonus( live: Boolean ): F[String] =
    readInstructions( live )
      .flatMap( instr => instr.evalMovesBonus.leftMap( Error( _ ) ).liftTo[F] )
}

object Aoc5 {
  object parsers {
    val crate: Parser[Crate] =
      Parser.anyChar
        .between( Parser.char( '[' ), Parser.char( ']' ) )
        .map( Crate( _ ) )

    val emptyCrate: Parser[Unit] = Parser.string( "   " )

    val crates: Parser[NonEmptyVector[Option[Crate]]] =
      crate
        .map( _.some )
        .orElse( emptyCrate.as( none[Crate] ) )
        .repSep( Parser.char( ' ' ) )
        .map( _.toNev )

    def stackIndices0( n: Int ): Parser0[Int] =
      Parser.end
        .as( n )
        .orElse( Parser.string( s"  ${n + 1} " ) *> Parser.defer0( stackIndices0( n + 1 ) ) )

    val stackIndices: Parser[Int] = Parser.string( " 1 " ) *> stackIndices0( 1 )

    val number: Parser[Int] =
      Numbers.nonNegativeIntString.mapFilter( _.toIntOption )

    val moveLine: Parser[Move] = (
      Parser.string( "move " ) *> number,
      Parser.string( " from " ) *> number,
      Parser.string( " to " ) *> number
    ).mapN( ( c, s, d ) => Move( c, s - 1, d - 1 ) )
  }

  case class Crate( key: Char )
  object Crate {
    implicit val crateShow: Show[Crate] = Show.show( c => s"[${c.key}]" )
  }

  case class Move( count: Int, from: Int, to: Int )
  object Move {
    implicit val moveShow: Show[Move] = semiauto.show[Move]
  }

  sealed abstract class Accumulator extends Product {
    def readLine( str: String ): Either[String, Accumulator]
    def validate: Either[String, Instructions] = Left( "No moves read" )
  }

  def toStacks( stackCount: Int, lines: List[NonEmptyVector[Option[Crate]]] ): Vector[List[Crate]] = {
    lines.foldLeft( Vector.fill( stackCount )( List.empty[Crate] ) )(
      ( acc, line ) =>
        acc.zipWithIndex.map {
          case ( stack, ix ) =>
            val crate: Option[Crate] = line.toVector.lift( ix ).flatten
            crate.fold( stack )( _ :: stack )
        }
    )
  }

  case class ReadingStacks( stackLines: List[NonEmptyVector[Option[Crate]]] ) extends Accumulator {
    override def readLine( str: String ): Either[String, Accumulator] =
      parsers.crates
        .eitherOr( parsers.stackIndices )
        .map {
          case Left( stackCount ) => ReadStacks( toStacks( stackCount, stackLines ) )
          case Right( crateLine ) => ReadingStacks( crateLine :: stackLines )
        }
        .parseAll( str )
        .leftMap( Data.formatError( str, _ ) )
  }

  case class ReadStacks( stacks: Vector[List[Crate]] ) extends Accumulator {
    override def readLine( str: String ): Either[String, Accumulator] =
      if (str.trim.isEmpty)
        Right( ReadingMoves( stacks, Nil ) )
      else
        Left( "Expected empty line after crates" )
  }

  case class ReadingMoves( stacks: Vector[List[Crate]], moves: List[Move] ) extends Accumulator {
    override def readLine( str: String ): Either[String, Accumulator] =
      if (str.trim.isEmpty) Right( this )
      else
        parsers.moveLine
          .parseAll( str )
          .leftMap( Data.formatError( str, _ ) )
          .map( move => ReadingMoves( stacks, move :: moves ) )

    override def validate: Either[String, Instructions] =
      Right( Instructions( stacks, moves.reverse ) )
  }

  case class Instructions(
      stacks: Vector[List[Crate]],
      moves: List[Move]
  ) {
    private def updateStacks(
        srcStack: List[Crate],
        destStack: List[Crate],
        num: Int
    ): Either[String, ( List[Crate], List[Crate] )] =
      if (srcStack.size < num) Left( s"Cannot move $num from $srcStack" )
      else
        Right(
          (
            srcStack.drop( num ),
            srcStack.take( num ).reverse ++ destStack
          )
        )

    def evalMove( move: Move, stacks: Vector[List[Crate]] )(
        moveMethod: ( List[Crate], List[Crate], Int ) => Either[String, ( List[Crate], List[Crate] )]
    ): Either[String, Vector[List[Crate]]] = {
      for {
        _                     <- if (move.from == move.to) Left( s"Invalid move from ${move.from} to itself" ) else Right( () )
        srcStack              <- stacks.lift( move.from ).toRight( s"Invalid move from index ${move.from}" )
        destStack             <- stacks.lift( move.to ).toRight( s"Invalid move from index ${move.to}" )
        ( nextSrc, nextDest ) <- moveMethod( srcStack, destStack, move.count )
      } yield stacks.updated( move.from, nextSrc ).updated( move.to, nextDest )

    }

    def evalMoves: Either[String, String] =
      moves
        .foldLeftM( stacks )( ( acc, move ) => evalMove( move, acc )( updateStacks ) )
        .map( _.mapFilter( _.headOption ).foldMap( _.key.toString ) )

    def updateStacksBonus(
        srcStack: List[Crate],
        destStack: List[Crate],
        num: Int
    ): Either[String, ( List[Crate], List[Crate] )] =
      if (srcStack.size < num) Left( s"Cannot move $num from $srcStack" )
      else
        Right(
          (
            srcStack.drop( num ),
            srcStack.take( num ) ++ destStack
          )
        )

    def evalMovesBonus: Either[String, String] =
      moves
        .foldLeftM( stacks )( ( acc, move ) => evalMove( move, acc )( updateStacksBonus ) )
        .map( _.mapFilter( _.headOption ).foldMap( _.key.toString ) )
  }

  object Instructions {
    implicit val instructionsShow: Show[Instructions] = {
      semiauto.show[Instructions]
    }
  }

}

package aoc22

import cats.Monoid
import cats.Show
import cats.derived.semiauto
import cats.effect.Sync
import cats.parse.Parser
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.syntax.traverse._
import enumeratum.Enum
import enumeratum.EnumEntry
import scala.annotation.tailrec

class Aoc11[F[_]: Sync]( srcFile: String ) extends Day.Of[F]( srcFile ) {
  import Aoc11._

  def getMonkeys: F[Monkeys[Int]] =
    lines
      .chunkN( 6, allowFewer = true )
      .evalScan( Vector.empty[Monkey[Int]] )(
        ( acc, chunk ) =>
          Monkey
            .parse( acc.size, chunk.toVector )
            .map( acc :+ _ )
            .into[F]
      )
      .compile
      .lastOrError
      .flatMap( ms => Monkeys.validate( ms ).into[F] )

  @tailrec
  final def playRounds[A]( monkeys: Monkeys[A], count: Int ): Long =
    if (count == 0) monkeys.business
    else playRounds( monkeys.round, count - 1 )

  override def basic: F[String] =
    getMonkeys.map( playRounds( _, 20 ).toString )

  override def bonus: F[String] =
    getMonkeys
      .map( Monkeys.toBonus )
      .map( playRounds( _, 10000 ).toString )
}

object Aoc11 {

  sealed abstract class Op( val key: Char, val apply: ( Int, Int ) => Int ) extends EnumEntry with Product
  object Op extends Enum[Op] {
    case object Add  extends Op( '+', ( x, y ) => x + y )
    case object Mult extends Op( '*', ( x, y ) => x * y )

    override val values: Vector[Op] = findValues.toVector
  }

  sealed abstract class OpArg( val apply: Int => Int, override val toString: String ) extends Product

  case object Old            extends OpArg( identity, "old" )
  case class Const( n: Int ) extends OpArg( Function.const( n ), n.toString )

  case class MonkeyRules(
      op: Op,
      arg1: OpArg,
      arg2: OpArg,
      testDiv: Int,
      targetIfTrue: Int,
      targetIfFalse: Int
  ) {
    // BASIC
    private def worryBasic( item: Int ): Int =
      op.apply( arg1.apply( item ), arg2.apply( item ) ) / 3

    private def targetBasic( item: Int ): Int =
      if (item % testDiv == 0) targetIfTrue else targetIfFalse

    def inspectBasic( item: Int ): Round[Int] = {
      val nextWorry = worryBasic( item )
      Round( 1, Map( targetBasic( nextWorry ) -> Vector( nextWorry ) ) )
    }

    // BONUS
    private def worryBonus( item: Item ): Item =
      item.run( op, arg1, arg2 )

    private def targetBonus( item: Item ): Int =
      if (item.mod( testDiv ) == 0) targetIfTrue else targetIfFalse

    def inspectBonus( item: Item ): Round[Item] = {
      val nextItem = worryBonus( item )
      Round( 1, Map( targetBonus( nextItem ) -> Vector( nextItem ) ) )
    }

    // ad hoc polymorphism with type class rather than overloading, sure
    def inspect[A]( item: A )( implicit A: ApplyRules[A] ): Round[A] =
      A.inspect( this, item )
  }

  sealed trait ApplyRules[A] {
    def inspect( rules: MonkeyRules, a: A ): Round[A]
  }

  object ApplyRules {
    implicit val applyRulesBasic: ApplyRules[Int] = new ApplyRules[Int] {
      override def inspect( rules: MonkeyRules, a: Int ): Round[Int] = rules.inspectBasic( a )
    }

    implicit val applyRulesBonus: ApplyRules[Item] = new ApplyRules[Item] {
      override def inspect( rules: MonkeyRules, a: Item ): Round[Item] = rules.inspectBonus( a )
    }
  }

  case class Round[A](
      inspected: Int,
      thrown: Map[Int, Vector[A]]
  )

  object Round {
    implicit def roundMonoid[A]: Monoid[Round[A]] = semiauto.monoid[Round[A]]
  }

  case class Monkey[A: ApplyRules]( items: Vector[A], rules: MonkeyRules ) {
    def round: Round[A]                            = items.foldMap( rules.inspect( _ ) )
    def receive( moreItems: Vector[A] ): Monkey[A] = copy( items = items ++ moreItems )
  }

  object Monkey {
    def parse( ix: Int, lines: Vector[String] ): Either[String, Monkey[Int]] =
      (
        parsers.monkeyHeader( ix ).parseAll( lines( 0 ) ),
        parsers.startingItems.parseAll( lines( 1 ) ),
        parsers.operation.parseAll( lines( 2 ) ),
        parsers.test.parseAll( lines( 3 ) ),
        parsers.target( "true" ).parseAll( lines( 4 ) ),
        parsers.target( "false" ).parseAll( lines( 5 ) )
      ).mapN {
          case ( _, starting, ( arg1, op, arg2 ), div, targetIfTrue, targetIfFalse ) =>
            Monkey(
              starting,
              MonkeyRules( op, arg1, arg2, div, targetIfTrue, targetIfFalse )
            )
        }
        .leftMap( formatError( lines.mkString_( "\n" ), _ ) )

    implicit def monkeyShow[A: Show]: Show[Monkey[A]] = Show.show(
      monkey => s"""ITEMS: ${monkey.items.mkString_( ", " )}
                   |OP: ${monkey.rules.arg1} ${monkey.rules.op.key} ${monkey.rules.arg2}
                   |TARGET: ${monkey.rules.targetIfTrue} if divisible by ${monkey.rules.testDiv}
                   |        ${monkey.rules.targetIfFalse} otherwise""".stripMargin
    )
  }

  case class Monkeys[A: ApplyRules]( monkeys: Vector[Monkey[A]], inspections: Map[Int, Int] ) {
    def roundOf( ix: Int ): Monkeys[A] = {
      val Round( inspected, passes ) = monkeys( ix ).round

      val newInspections: Map[Int, Int] = inspections |+| Map( ix -> inspected )

      val newMonkeys: Vector[Monkey[A]] =
        monkeys.zipWithIndex.map {
          case ( m, i ) =>
            if (i == ix)
              m.copy( items = Vector.empty )
            else
              m.receive( passes.getOrElse( i, Vector.empty ) )
        }

      Monkeys( newMonkeys, newInspections )
    }

    def round: Monkeys[A] =
      monkeys.indices.foldLeft( this )( _.roundOf( _ ) )

    def business: Long =
      inspections.values.toVector.sortBy( -_ ).take( 2 ).map( _.toLong ).product
  }

  object Monkeys {
    def validate( monkeys: Vector[Monkey[Int]] ): Either[String, Monkeys[Int]] = {
      def validTarget( ix: Int ): Either[String, Unit] =
        if (ix >= 0 && ix < monkeys.size) Right( () )
        else Left( s"invalid target $ix" )

      monkeys
        .traverseWithIndexM {
          case ( monkey, ix ) =>
            (
              validTarget( monkey.rules.targetIfTrue ) *>
                validTarget( monkey.rules.targetIfFalse )
            ).leftMap( s"Monkey $ix: " + _ )
              .as( monkey )
        }
        .map(
          Monkeys( _, Map.empty )
        )
    }

    def toBonus( monkeys: Monkeys[Int] ): Monkeys[Item] = {
      val mods: Vector[Int] = monkeys.monkeys.map( _.rules.testDiv ).distinct

      Monkeys(
        monkeys.monkeys.map( monkey => Monkey[Item]( monkey.items.map( n => Item.apply( n, mods ) ), monkey.rules ) ),
        monkeys.inspections
      )
    }

    implicit def monkeysShow[A: Show]: Show[Monkeys[A]] = Show.show(
      monkeys =>
        monkeys.monkeys.zipWithIndex
          .map {
            case ( m, i ) =>
              s"""Monkey $i:
                 |${m.show}
                 |INSPECTED: ${monkeys.inspections.getOrElse( i, 0 )}""".stripMargin
          }
          .mkString( "\n\n" )
    )

  }

  case class Item( mods: Map[Int, Int] ) {
    def run( op: Op, arg1: OpArg, arg2: OpArg ): Item =
      Item( mods.map {
        case ( k, v ) =>
          k -> op.apply( arg1.apply( v ), arg2.apply( v ) ) % k
      } )

    def mod( n: Int ): Int = mods( n )
  }

  object Item {
    def apply( init: Int, mods: Vector[Int] ): Item =
      Item( mods.map( m => ( m, init % m ) ).toMap )
  }

  object parsers {
    import cats.parse.Numbers.nonNegativeIntString
    import cats.parse.Rfc5234.wsp

    val op: Parser[Op] = Op.values.foldMapK( op => Parser.char( op.key ).as( op ) )

    private val opArgOld: Parser[Old.type] = Parser.string( "old" ).as( Old )
    private val opArgConst: Parser[Const]  = nonNegativeIntString.mapFilter( _.toIntOption ).map( Const )

    val opArg: Parser[OpArg] = opArgOld.orElse( opArgConst )

    def monkeyHeader( ix: Int ): Parser[Unit] =
      Parser.string( s"Monkey $ix:" ).void

    val startingItems: Parser[Vector[Int]] =
      Parser.string( "Starting items: " ) *>
        nonNegativeIntString
          .mapFilter( _.toIntOption )
          .repSep( Parser.string( ", " ) )
          .map( _.toList.toVector )
          .orElse( Parser.string( "Starting items:" ).as( Vector.empty[Int] ) )

    val operation: Parser[( OpArg, Op, OpArg )] =
      Parser.string( "Operation: new = " ) *>
        (
          opArg <* wsp,
          op <* wsp,
          opArg
        ).tupled

    val test: Parser[Int] =
      Parser.string( "Test: divisible by " ) *> nonNegativeIntString.mapFilter( _.toIntOption )

    def target( cond: String ): Parser[Int] =
      Parser.string( s"If $cond: throw to monkey " ) *> nonNegativeIntString.mapFilter( _.toIntOption )

  }
}

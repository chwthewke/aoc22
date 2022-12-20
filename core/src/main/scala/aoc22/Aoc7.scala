package aoc22

import cats.effect.Sync
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._

class Aoc7[F[_]: Sync] extends Day.N[F]( 7 ) {
  import Aoc7._

  private def getFileSystem( live: Boolean ): F[FileSystem] =
    lines( live )
      .through( Data.parseLines( parsers.line ) )
      .evalScan( Init: Scan )( ( s, line ) => s.readLine( line ).into[F] )
      .map( _.fileSystem )
      .compile
      .lastOrError

  override def basic( live: Boolean ): F[String] =
    getFileSystem( live ).map(
      fs =>
        smallDirectories( fs, 100000L )
          .foldMap( _._2 )
          .toString
    )

  def smallDirectories( fileSystem: FileSystem, limit: Long ): Vector[( List[String], Long )] =
    fileSystem.dirSizes.filter { case ( _, sz ) => sz <= limit }.toVector

  override def bonus( live: Boolean ): F[String] =
    getFileSystem( live ).map(
      fs => directoryToDelete( fs, 70000000L, 30000000L )._2.toString
    )

  def directoryToDelete( fileSystem: FileSystem, totalSpace: Long, reqSpace: Long ): ( List[String], Long ) = {

    val usedSpace: Long = fileSystem.dirSizes.getOrElse( Nil, 0L )

    val reclaimAtLeast: Long = 0L.max( reqSpace - totalSpace + usedSpace )

    val dirToDelete = fileSystem.dirSizes.toVector.filter( _._2 >= reclaimAtLeast ).minBy( _._2 )

    dirToDelete
  }

}

object Aoc7 {
  case class FileSystem( files: Vector[File] ) {
    def addFile( cwd: List[String], file: LsFile ): FileSystem =
      FileSystem( files :+ File( cwd, file.name, file.size ) )

    lazy val dirSizes: Map[List[String], Long] =
      files.foldMap( f => f.dir.tails.map( d => ( d, f.size ) ).toMap )
  }

  case class File( dir: List[String], name: String, size: Long )

  sealed abstract class Scan {
    def readLine( line: Line ): Either[String, Scan]
    def fileSystem: FileSystem
  }

  case object Init extends Scan {
    override val fileSystem: FileSystem = FileSystem( Vector.empty )

    override def readLine( line: Line ): Either[String, Scan] =
      if (line == Cd( Slash )) {
        Right( ReadingCommand( Nil, fileSystem ) )
      } else Left( s"Unexpected initial line: $line" )
  }

  abstract class Reading extends Scan {
    def cwd: List[String]
    def fileSystem: FileSystem

    protected def readCommand( command: Command ): Scan =
      command match {
        case Cd( Slash )         => ReadingCommand( Nil, fileSystem )
        case Cd( DotDot )        => ReadingCommand( cwd.tail, fileSystem )
        case Cd( CdName( dir ) ) => ReadingCommand( dir :: cwd, fileSystem )
        case Ls                  => ReadingLs( cwd, fileSystem )
      }
  }

  case class ReadingCommand( cwd: List[String], fileSystem: FileSystem ) extends Reading {
    override def readLine( line: Line ): Either[String, Scan] =
      line match {
        case cmd: Command => Right( readCommand( cmd ) )
        case _            => Left( s"Expected command, got $line" )
      }
  }
  case class ReadingLs( cwd: List[String], fileSystem: FileSystem ) extends Reading {
    override def readLine( line: Line ): Either[String, Scan] =
      line match {
        case cmd: Command => Right( readCommand( cmd ) )
        case file: LsFile => Right( ReadingLs( cwd, fileSystem.addFile( cwd, file ) ) )
        case _: LsDir     => Right( this )
      }
  }

  sealed abstract class CdTarget    extends Product
  case object Slash                 extends CdTarget
  case object DotDot                extends CdTarget
  case class CdName( path: String ) extends CdTarget

  sealed abstract class Line extends Product

  sealed abstract class Command     extends Line
  case class Cd( target: CdTarget ) extends Command
  case object Ls                    extends Command

  sealed abstract class LsItem                  extends Line
  case class LsFile( size: Long, name: String ) extends LsItem
  case class LsDir( name: String )              extends LsItem

  object parsers {
    import cats.parse.Rfc5234.alpha
    import cats.parse.Rfc5234.digit
    import cats.parse.Rfc5234.wsp

    val dotDot: Parser[DotDot.type] = Parser.string( ".." ).as( DotDot )
    val slash: Parser[Slash.type]   = Parser.string( "/" ).as( Slash )

    val name: Parser[String] =
      Parser.string( alpha.orElse( digit ).orElse( Parser.charIn( "_-." ) ).rep )

    val cd: Parser[Cd] =
      (Parser.string( "cd" ) *> wsp.rep *> dotDot.orElse( slash ).orElse( name.map( CdName ) )).map( Cd )

    val ls: Parser[Ls.type] =
      Parser.string( "ls" ).as( Ls )

    val command: Parser[Command] =
      Parser.string( "$" ) *> wsp.rep *> (cd orElse ls)

    val lsFile: Parser[LsFile] =
      ( Numbers.bigInt.map( _.toLong ) <* wsp.rep, name ).mapN( LsFile )

    val lsDir: Parser[LsDir] =
      (Parser.string( "dir" ) *> wsp.rep *> name).map( LsDir )

    val lsItem: Parser[LsItem] =
      lsDir.orElse( lsFile )

    val line: Parser[Line] =
      command.orElse( lsItem )
  }

}

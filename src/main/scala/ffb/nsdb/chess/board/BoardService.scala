package ffb.nsdb.chess.board

import zio.{Task, UIO, ULayer, ZIO, ZLayer}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import ffb.nsdb.chess.{Color, Piece, Position}

sealed trait Board {
  def setBoardPositions(fen: String): Task[Board]

  def setBoardToFenString: Task[String]

  def getPieceAtPosition(position: Position): Task[Option[Piece]]

  def isKingInCheck(color: Color): UIO[Boolean]

  def getPiecesByColor(color: Color): Task[Seq[Piece]]

  def getPiecesByColorAndType(color: Color, pieceType: Piece): Task[Seq[Piece]]

  def toArray: Task[IndexedSeq[String]]
}

object Board {
  def setBoardPositions(fen: String): ZIO[Board, Throwable, Board] =
    ZIO.serviceWithZIO[Board](_.setBoardPositions(fen))

  def setBoardToFenString(): ZIO[Board, Throwable, String] =
    ZIO.serviceWithZIO[Board](_.setBoardToFenString)

  def getPieceAtPosition(position: Position): ZIO[Board, Throwable, Option[Piece]] =
    ZIO.serviceWithZIO[Board](_.getPieceAtPosition(position))

  def isKingInCheck(color: Color): ZIO[Board, Nothing, Boolean] =
    ZIO.serviceWithZIO[Board](_.isKingInCheck(color))

  def getPiecesByColor(color: Color): ZIO[Board, Throwable, Seq[Piece]] =
    ZIO.serviceWithZIO[Board](_.getPiecesByColor(color))

  def getPiecesByColorAndType(color: Color, pieceType: Piece): ZIO[Board, Throwable, Seq[Piece]] =
    ZIO.serviceWithZIO[Board](_.getPiecesByColorAndType(color, pieceType))

  def toArray: ZIO[Board, Throwable, IndexedSeq[String]] =
    ZIO.serviceWithZIO[Board](_.toArray)

  implicit val encoder: JsonEncoder[Board] = DeriveJsonEncoder.gen[Board].contramap {
    case board: MailBoxBoard => {
      val newBoard = board.squares.collect {
        case square: Occupied => square
        case square: Unoccupied => square
      }
      MailBoxBoard(newBoard)
    }
  }
  implicit val decoder: JsonDecoder[Board] = DeriveJsonDecoder.gen[Board]
}

final case class MailBoxBoard(squares: IndexedSeq[Square]) extends Board {

  // These values correspond to how deep into the 120 index array the actual chess board
  // square begin and end. In the Mailbox Board, there is a 2 space buffer around the actual
  // chess board to make it more clear when a move goes outside the boundary of the board.
  // ############
  // ############
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ##OOOOOOOO##
  // ############
  // ############
  private val StartOffset: Int = 24 // The offset into the mailbox board to where the real board starts
  private val RowPadding: Int = 2 // The padding on either end of the rows

  private def getPositionFromIndex(index: Int): Position = {
    // Gets the practical index in a 64 square board from the actual index in the mailbox array
    def getChessBoardIndex(i: Int): Int = {
      // Integer division by 12 gives the row of the board and then I subtract the number of padding rows
      // preceding the first row of real positions
      val row: Int = i / 12 - 2

      i - StartOffset - RowPadding - (2 * row * RowPadding)
    }

    Position(getChessBoardIndex(index))
  }

  def setBoardPositions(fen: String): Task[MailBoxBoard] = for {
    boardString <- ZIO.attempt {
      fen
        .split("/")
        .map("##" + _ + "##")
        .reduce(_ + _)
        .flatMap { ch =>
          if (ch.isDigit) "*" * ch.asDigit
          else ch.toString
        }
    }
    rowPadding = "#" * 24
    mailboxBoard = rowPadding + boardString + rowPadding
    squares <- ZIO.attempt {
      mailboxBoard.zipWithIndex
        .foldLeft(IndexedSeq.fill(120)(OutOfBounds: Square)) {
          (acc: IndexedSeq[Square], pair: (Char, Int)) => {
            val i = pair._2

            pair._1 match {
              case c if c.isLetter =>
                val position = getPositionFromIndex(i)
                val piece = Piece(c)
                acc.updated(i, Occupied(piece, position))

              case c if c == '*' =>
                val position = getPositionFromIndex(i)
                acc.updated(i, Unoccupied(position))

              case _ => acc
            }
          }
        }
    }
  } yield MailBoxBoard(squares)

  def setBoardToFenString: Task[String] = ???

  def getPieceAtPosition(position: Position): Task[Option[Piece]] = {
    val occupiedSquares = squares.collect { case square: Occupied => square }

    ZIO.succeed {
      occupiedSquares.find(_.position == position) match {
        case Some(Occupied(piece, _)) => Some(piece)
        case _ => None
      }
    }
  }

  def isKingInCheck(color: Color): UIO[Boolean] = ???

  def getPiecesByColor(color: Color): Task[Seq[Piece]] = ZIO.succeed {
    squares.collect { case square: Occupied if square.piece.color == color => square.piece }
  }

  def getPiecesByColorAndType(color: Color, pieceType: Piece): Task[Seq[Piece]] = {
    getPiecesByColor(color) flatMap { piecesByColor =>
      ZIO.succeed(piecesByColor.collect { case piece if pieceType.materialWeight == piece.materialWeight => piece })
    }
  }

  def toArray: Task[IndexedSeq[String]] = ZIO.succeed {
    squares.collect {
      case square: Occupied => square.piece.toString
      case _: Unoccupied    => ""
    }
  }

  override def toString: String =
    squares.grouped(12)
      .toList
      .foldLeft("") { (acc: String, lst: IndexedSeq[Square]) =>
        acc + lst.toString + "\n"
      }
}

object MailBoxBoard {
  val encoder: JsonEncoder[MailBoxBoard] =
    DeriveJsonEncoder.gen[MailBoxBoard] contramap { board =>
      val newBoard = board.squares.collect {
        case square: Occupied => square
        case square: Unoccupied => square
      }
      MailBoxBoard(newBoard)
    }
  val layer: ULayer[MailBoxBoard] =
    ZLayer.succeed(MailBoxBoard(IndexedSeq.fill(120)(OutOfBounds: Square)))
}

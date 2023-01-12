package ffb.nsdb.chess.board

import ffb.nsdb.chess._

sealed trait Board
object Board {
  def getBoardFromString[A](fenString: String)(implicit bs: BoardService[A]): A =
    bs.setBoardPositions(fenString)
}

final case class SquareBoard(squares: Vector[Option[Piece]]) extends Board {
  def apply(i: Int): Option[Piece] = squares(i)
}

final case class PieceBoard(white: Vector[Bitboard], black: Vector[Bitboard])
    extends Board

trait BoardService[A] {
  def setBoardPositions(fenBoard: String): A
}

object BoardService {
  implicit val squareBoardService: BoardService[SquareBoard] =
    new BoardService[SquareBoard] {
      def setBoardPositions(fenBoard: String): SquareBoard = {
        val boardString = fenBoard
          .split("/")
          .map(_.reverse)
          .reduce(_ + _) flatMap { ch =>
          if (ch.isDigit) "*" * ch.asDigit
          else ch.toString
        }

        val pieces =
          boardString.reverse.zipWithIndex.foldLeft(
            Vector.fill(64)(None: Option[Piece])
          ) { (acc: Vector[Option[Piece]], pair: (Char, Int)) =>
            {
              val i = pair._2
              pair._1 match {
                case c if c.isLower => acc.updated(i, Some(Piece(c, Black)))
                case c if c.isUpper => acc.updated(i, Some(Piece(c, White)))
                case _              => acc
              }
            }
          }
        SquareBoard(pieces)
      }
    }

  implicit val pieceBoardService: BoardService[PieceBoard] =
    new BoardService[PieceBoard] {
      def setBoardPositions(fenBoard: String): PieceBoard = {
        val boardString = fenBoard.replace("/", "") flatMap { ch =>
          if (ch.isDigit) "*" * ch.asDigit
          else ch.toString
        }

        val white =
          boardString.zipWithIndex.foldLeft(Vector.fill(6)(Bitboard(0L))) {
            (acc: Vector[Bitboard], pair: (Char, Int)) =>
              val mask = 1L << pair._2

              pair._1 match {
                case 'P' => acc.updated(Pawn.value, acc(Pawn.value) | mask)
                case 'N' => acc.updated(Knight.value, acc(Knight.value) | mask)
                case 'B' => acc.updated(Bishop.value, acc(Bishop.value) | mask)
                case 'R' => acc.updated(Rook.value, acc(Rook.value) | mask)
                case 'Q' => acc.updated(Queen.value, acc(Queen.value) | mask)
                case 'K' => acc.updated(King.value, acc(King.value) | mask)
                case _   => acc
              }
          }

        val black =
          boardString.zipWithIndex.foldLeft(Vector.fill(6)(Bitboard(0L))) {
            (acc: Vector[Bitboard], pair: (Char, Int)) =>
              val mask = 1L << pair._2

              pair._1 match {
                case 'p' => acc.updated(Pawn.value, acc(Pawn.value) | mask)
                case 'n' => acc.updated(Knight.value, acc(Knight.value) | mask)
                case 'b' => acc.updated(Bishop.value, acc(Bishop.value) | mask)
                case 'r' => acc.updated(Rook.value, acc(Rook.value) | mask)
                case 'q' => acc.updated(Queen.value, acc(Queen.value) | mask)
                case 'k' => acc.updated(King.value, acc(King.value) | mask)
                case _   => acc
              }
          }

        PieceBoard(white, black)
      }
    }
}

package ffb.nsdb.chess.board

import ffb.nsdb.chess._
import ffb.nsdb.chess.board.PieceBoard
import ffb.nsdb.chess.board.PieceBoard
import ffb.nsdb.chess.board.CastleRights

final case class GameState(
    pieces: PieceBoard,
    squares: SquareBoard,
    sideToMove: Color,
    castleRights: CastleRights,
    epTarget: Option[Square],
    halfMove: Int,
    fullMove: Int
)

trait GameStateService {
  def toString(bs: GameState): String
  def setBoardState(fenString: String): GameState
  def initializeBoard: GameState = {
    setBoardState("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  }
  def boardStateToFen(bs: GameState): String
  def getPieceAtSquare(bs: GameState, file: String, rank: Int): Piece
}

trait GameStateServiceImpl extends GameStateService {

  def setBoardState(fenString: String): GameState = {
    val fenProps = fenString.split(" ")

    val pieces = Board.setBoard[PieceBoard](fenProps(0))
    val squares = Board.setBoard[SquareBoard](fenProps(0))
    // val pieces = PieceBoard.setBoardPositions(fenProps(0))
    // val squares = SquareBoard.setBoardPositions(fenProps(0))

    val sideToMove = fenProps(1) match {
      case "w" => White
      case "b" => Black
      case  _  => throw new Error("Invalid side string provided")
    }

    val castleRights = CastleRights(fenProps(2))

    val epTarget = None

    val halfMoveClock = Option(fenProps(4).toInt) match {
      case Some(n) => n
      case None    => throw new Error("Half move clock was not a number")
    }

    val fullMove = Option(fenProps(5).toInt) match {
      case Some(n) => n
      case None    => throw new Error("Full move count value was not a number")
    }

    GameState(pieces, squares, sideToMove, castleRights, epTarget, halfMoveClock, fullMove)
  }

  def boardStateToFen(bs: GameState): String = "TO BE IMPLEMENTED"

  def getPieceAtSquare(bs: GameState, file: String, rank: Int): Piece =
    "abcdefgh".indexOf(file.toLowerCase) match {
      case -1 =>
        throw new Error("File string provided does not exist on the board")
      case i =>
        bs.squares((8 * (rank - 1)) + i)
    }

  def toString(bs: GameState): String = {
    (for (i <- 0 to 63) yield {
      val sq = 1L << i

      if ((sq & bs.pieces.white(Pawn.value).value) != 0) " P "
      else if ((sq & bs.pieces.white(Knight.value).value) != 0) " N "
      else if ((sq & bs.pieces.white(Bishop.value).value) != 0) " B "
      else if ((sq & bs.pieces.white(Rook.value).value) != 0) " R "
      else if ((sq & bs.pieces.white(Queen.value).value) != 0) " Q "
      else if ((sq & bs.pieces.white(King.value).value) != 0) " K "
      else if ((sq & bs.pieces.black(Pawn.value).value) != 0) " p "
      else if ((sq & bs.pieces.black(Knight.value).value) != 0) " n "
      else if ((sq & bs.pieces.black(Bishop.value).value) != 0) " b "
      else if ((sq & bs.pieces.black(Rook.value).value) != 0) " r "
      else if ((sq & bs.pieces.black(Queen.value).value) != 0) " q "
      else if ((sq & bs.pieces.black(King.value).value) != 0) " k "
      else " . "
    }).zipWithIndex.foldLeft("") { (acc, pair) =>
      if ((pair._2 + 1) % 8 == 0) acc + pair._1.toString + "\n"
      else acc + pair._1.toString
    }
  }
}

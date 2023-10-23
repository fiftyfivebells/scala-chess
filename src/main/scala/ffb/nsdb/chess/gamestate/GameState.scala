package ffb.nsdb.chess.gamestate

import ffb.nsdb.chess.board.Board
import ffb.nsdb.chess.{CastleAvailability, Color, Piece, Position}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}
import zio.{Task, UIO, ZIO, ZLayer}

final case class GameState(
  board: Board,
  sideToMove: Color,
  castleAvailability: CastleAvailability,
  epTarget: Option[Position],
  halfMove: Int,
  fullMove: Int
)

sealed trait GameStateService {

  def setGameStateFromFenString(fen: String): Task[GameState]

  def gameStateToFenString(gameState: GameState): Task[String]

  def getPieceAtPosition(position: Position, gameState: GameState): Task[Option[Piece]]

  def isKingInCheck(color: Color, gameState: GameState): UIO[Boolean]
}

object GameState {
  implicit val encoder: JsonEncoder[GameState] = DeriveJsonEncoder.gen[GameState]
  implicit val decoder: JsonDecoder[GameState] = DeriveJsonDecoder.gen[GameState]

  def setGameStateFromFenString(fen: String): ZIO[GameStateService, Throwable, GameState] =
    ZIO.serviceWithZIO[GameStateService](_.setGameStateFromFenString(fen))

  def gameStateToFenString(gameState: GameState): ZIO[GameStateService, Throwable, String] =
    ZIO.serviceWithZIO[GameStateService](_.gameStateToFenString(gameState))

  def getPieceAtPosition(position: Position, gameState: GameState): ZIO[GameStateService, Throwable, Option[Piece]] =
    ZIO.serviceWithZIO[GameStateService](_.getPieceAtPosition(position, gameState))

  def isKingInCheck(color: Color, gameState: GameState): ZIO[GameStateService, Nothing, Boolean] =
    ZIO.serviceWithZIO[GameStateService](_.isKingInCheck(color, gameState))
}

case class GameStateServiceImpl(board: Board) extends GameStateService {

  def setGameStateFromFenString(fen: String): Task[GameState] = for {
    fenProperties <- ZIO.succeed(fen.split(" "))
    _ <- ZIO.when(fenProperties.length != 6){
          ZIO.fail(new Exception(s"Incorrect format for fen string, received: $fen"))
        }
    Array(boardString, side, castleRights, epTarget, halfMove, fullMove) = fenProperties
    board         <- board.setBoardPositions(boardString)
    side          <- ZIO.attempt(Color(side))
    halfMoveClock <- ZIO.attempt(halfMove.toInt)
    fullMoveClock <- ZIO.attempt(fullMove.toInt)
    castleAvailability = CastleAvailability.fromString(castleRights)
    epOpt = epTarget match {
      case "-" => None
      case coords => Some(Position(coords))
    }
  } yield GameState(board, side, castleAvailability, epOpt, halfMoveClock, fullMoveClock)

  def gameStateToFenString(gameState: GameState): Task[String] = ???

  def getPieceAtPosition(position: Position, gameState: GameState): Task[Option[Piece]] =
    board.getPieceAtPosition(position)

  def isKingInCheck(color: Color, gameState: GameState): UIO[Boolean] = ???
}

object GameStateServiceImpl {

  val layer: ZLayer[Board, Nothing, GameStateService] = ZLayer {
    ZIO.service[Board] map { boardService =>
      GameStateServiceImpl(boardService)
    }
  }
}

sealed trait GameStateResponse
object GameStateResponse {
  implicit val encoder: JsonEncoder[GameStateResponse] = DeriveJsonEncoder.gen[GameStateResponse]
  implicit val decoder: JsonDecoder[GameStateResponse] = DeriveJsonDecoder.gen[GameStateResponse]
}

final case class GameStateSuccess(
                                   board: IndexedSeq[String],
                                   sideToMove: Color,
                                   castleAvailability: CastleAvailability,
                                   epTarget: Option[Position],
                                   halfMove: Int,
                                   fullMove: Int
                                 ) extends GameStateResponse
object GameStateSuccess {
  implicit val encoder: JsonEncoder[GameStateSuccess] = DeriveJsonEncoder.gen[GameStateSuccess]
  implicit val decoder: JsonDecoder[GameStateSuccess] = DeriveJsonDecoder.gen[GameStateSuccess]
}

final case class GameStateFailure(errorMessage: String) extends GameStateResponse
object GameStateFailure {
  implicit val encoder: JsonEncoder[GameStateFailure] = DeriveJsonEncoder.gen[GameStateFailure]
  implicit val decoder: JsonDecoder[GameStateFailure] = DeriveJsonDecoder.gen[GameStateFailure]
}

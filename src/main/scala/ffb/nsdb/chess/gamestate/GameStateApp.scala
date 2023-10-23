package ffb.nsdb.chess.gamestate

import zio.http.{->, /, Http, Method, Request, Response, Root}
import zio.json.EncoderOps
import zio.ZIO

import ffb.nsdb.chess.board.Board

object GameStateApp {

  private val InitialGameStateFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  private def getGameStateResponseFromString(fen: String): ZIO[GameStateService, Throwable, Response] = for {
    gameState <- GameState.setGameStateFromFenString(fen)
    boardArray <- gameState.board.toArray
    gameStateResponse = GameStateSuccess(
      boardArray,
      gameState.sideToMove,
      gameState.castleAvailability,
      gameState.epTarget,
      gameState.halfMove,
      gameState.fullMove
    )
  } yield Response.json(gameStateResponse.toJson)

  def apply(): Http[Board, Throwable, Request, Response] = Http.collectZIO[Request] {
    case Method.GET -> Root / "initializeBoard" => getGameStateResponseFromString(InitialGameStateFen)
    case Method.GET -> Root / "setBoardToFen" / fen => getGameStateResponseFromString(fen)
  }.provideLayer(GameStateServiceImpl.layer)
}

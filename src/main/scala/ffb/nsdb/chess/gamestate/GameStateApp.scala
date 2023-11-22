package ffb.nsdb.chess.gamestate

import zio.http.{->, /, Http, Method, Request, Response, Root, Status}
import zio.json.EncoderOps
import zio.ZIO
import ffb.nsdb.chess.board.Board
import ffb.nsdb.chess.gamestate.GameStateException.IncorrectFenFormatException

object GameStateApp {

  private val InitialGameStateFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  private val GameStateRoot = "gamestate"
  private val FenStringParam = "fenString"

  private def getGameStateResponseFromString(fen: String): ZIO[GameStateService, Throwable, Response] = for {
    gameState <- GameState.setGameStateFromFenString(fen)
    _ <- ZIO.debug(gameState.board)
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
    // TODO add better error handling to this endpoint
    case Method.GET -> Root / GameStateRoot / "initializeBoard" => getGameStateResponseFromString(InitialGameStateFen)

    case req@Method.GET -> Root / GameStateRoot / "setBoardToFen" =>
      req.url.queryParams.get(FenStringParam).toList.flatten.headOption match {
        case Some(fen) =>
          getGameStateResponseFromString(fen)
            .catchAll {
              case IncorrectFenFormatException => ZIO.succeed(Response.text(IncorrectFenFormatException.failReason))
              // TODO make the error-handling more fine-grained, there are definitely more expected exceptions than this
              case e@_ => ZIO.succeed(Response.text(s"Unexpected exception: ${e.getMessage}"))
            }

        case None =>
          ZIO.debug(s"Request requires a parameter fenString, but did not receive one")
            .zipRight(ZIO.succeed(Response.text("Got a bad fen string").withStatus(Status.BadRequest)))
      }


  }.provideLayer(GameStateServiceImpl.layer)
}

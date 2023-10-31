package ffb.nsdb.chess

import zio.{ZIO, ZIOAppDefault}
import zio.http.Server

import ffb.nsdb.chess.board.MailBoxBoard
import ffb.nsdb.chess.gamestate.GameStateApp

object Main extends ZIOAppDefault {
  val Port: Int = 8080

  override val run =
    (for {
      _ <- ZIO.log(s"Starting server on port $Port")
      _ <- Server.serve(GameStateApp().withDefaultErrorResponse)
    } yield ())
      .provide(
        MailBoxBoard.layer,
        Server.defaultWithPort(Port)
      ).exitCode
}

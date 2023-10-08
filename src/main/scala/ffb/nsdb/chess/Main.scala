package ffb.nsdb.chess

import zio.{Console, ExitCode, URIO, ZIOAppDefault}
import zio.http.{->, /, App, Http, Method, Request, Response, Root, Server}

object Main extends ZIOAppDefault {
  val Port: Int = 8080

  val app: App[Any] =
    Http.collect[Request] {
      case Method.GET -> Root / "bestMove" => Response.text("TO BE IMPLEMENTED: best move returned here")
    }

  override val run: URIO[Any, ExitCode] =
    Console.printLine(s"Starting server on port $Port")
      .provide(Server.serve(app).defaultWithPort(Port))
      .exitCode
}

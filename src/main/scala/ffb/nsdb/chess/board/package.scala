package ffb.nsdb.chess

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

package object board {

  sealed trait Square
  case class Occupied(piece: Piece, position: Position) extends Square {
    override def toString: String = s" ${piece.toString}"
  }
  case object OutOfBounds extends Square {
    override def toString: String = " X "
  }
  case class Unoccupied(position: Position) extends Square {
    override def toString: String = s" _ "
  }

  object Square {
    implicit val encoder: JsonEncoder[Square] = DeriveJsonEncoder.gen[Square]
    implicit val decoder: JsonDecoder[Square] = DeriveJsonDecoder.gen[Square]
  }
}

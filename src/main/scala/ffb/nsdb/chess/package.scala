package ffb.nsdb

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder, jsonField}

import scala.util.{Failure, Success, Try}


package object chess {

  sealed trait Color

  object Color {

    implicit val encoder: JsonEncoder[Color] =
      JsonEncoder[String].contramap(_.toString)
    implicit val decoder: JsonDecoder[Color] =
      JsonDecoder[String].map(color => Color(color.head.toString))

    def apply(color: String): Color = {
      val lowercaseColor = color.toLowerCase
      require(lowercaseColor == "w" || lowercaseColor == "b", s"Color should be either w or b, received $lowercaseColor")

      if (lowercaseColor == "w") White else Black
    }
  }
  case object White extends Color
  case object Black extends Color

  sealed trait PieceType

  object PieceType {
    implicit val encoder: JsonEncoder[PieceType] = DeriveJsonEncoder.gen[PieceType]
    implicit val decoder: JsonDecoder[PieceType] = DeriveJsonDecoder.gen[PieceType]
  }
  case object Pawn extends PieceType
  case object Knight extends PieceType
  case object Bishop extends PieceType
  case object Rook extends PieceType
  case object Queen extends PieceType
  case object King extends PieceType

  sealed trait Piece {
    def materialWeight: Int
    @jsonField("color")
    def color: Color
    @jsonField("pieceType")
    def pieceType: PieceType

    override def toString: String = this match {
      case Pawn(color) => s"${if (color == White) "P" else "p"}"
      case Knight(color) => s"${if (color == White) "N" else "n"}"
      case Bishop(color) => s"${if (color == White) "B" else "b"}"
      case Rook(color) => s"${if (color == White) "R" else "r"}"
      case Queen(color) => s"${if (color == White) "Q" else "q"}"
      case King(color) => s"${if (color == White) "K" else "k"}"
    }
  }

  case class Pawn(color: Color) extends Piece {
    val materialWeight: Int = 10
    val pieceType: PieceType = Pawn
  }
  case class Knight(color: Color) extends Piece {
    val materialWeight: Int = 30
    val pieceType: PieceType = Knight
  }
  case class Bishop(color: Color) extends Piece {
    val materialWeight: Int = 35
    val pieceType: PieceType = Bishop
  }
  case class Rook(color: Color) extends Piece {
    val materialWeight: Int = 50
    val pieceType: PieceType = Rook
  }
  case class Queen(color: Color) extends Piece {
    val materialWeight: Int = 80
    val pieceType: PieceType = Queen
  }
  case class King(color: Color) extends Piece {
    val materialWeight: Int = 1000
    val pieceType: PieceType = King
  }

  object Piece {

    implicit val encoder: JsonEncoder[Piece] = DeriveJsonEncoder.gen[Piece]
    implicit val decoder: JsonDecoder[Piece] = DeriveJsonDecoder.gen[Piece]
    def apply(ch: Char): Piece = {
      val color = if (ch.isUpper) White else Black

      ch.toLower match {
        case 'p' => Pawn(color)
        case 'n' => Knight(color)
        case 'b' => Bishop(color)
        case 'r' => Rook(color)
        case 'q' => Queen(color)
        case 'k' => King(color)
        case _   => throw new Exception("That character does not correspond to a piece.")
      }
    }
  }

  final case class CastleAvailability(whiteKing: Boolean, blackKing: Boolean, whiteQueen: Boolean, blackQueen: Boolean) {

    def canCastleKingside(color: Color): Boolean = color match {
      case White => whiteKing
      case Black => blackKing
    }

    def canCastleQueenside(color: Color): Boolean = color match {
      case White => whiteQueen
      case Black => blackQueen
    }
  }

  object CastleAvailability {

    implicit val encoder: JsonEncoder[CastleAvailability] = DeriveJsonEncoder.gen[CastleAvailability]
    implicit val decoder: JsonDecoder[CastleAvailability] = DeriveJsonDecoder.gen[CastleAvailability]
    def fromString(castleString: String): CastleAvailability = {
      val whiteKing: Boolean = castleString.contains("K")
      val blackKing: Boolean = castleString.contains("k")
      val whiteQueen: Boolean = castleString.contains("Q")
      val blackQueen: Boolean = castleString.contains("q")

      CastleAvailability(whiteKing, blackKing, whiteQueen, blackQueen)
    }
  }

  case class Rank(value: Int) extends AnyRef {
    override def toString: String = value.toString
  }
  object Rank {

    implicit val encoder: JsonEncoder[Rank] =
      JsonEncoder[Int].contramap(_.value)
    implicit val decoder: JsonDecoder[Rank] =
      JsonDecoder[Int].map(Rank(_))
    def apply(v: Int): Rank = {
      if (v > 0 && v < 9)
        new Rank(v)
      else
        throw new Exception("Value should be an integer between 1 and 8")
    }
  }

  case class File(value: String) extends AnyRef {
    override def toString: String = value
  }
  object File {

    implicit val encoder: JsonEncoder[File] =
      JsonEncoder[String].contramap(_.value)
    implicit val decoder: JsonDecoder[File] =
      JsonDecoder[String].map(File(_))
    def apply(v: String): File = {
      val files = "abcdefgh".split("")
      if (files.contains(v.toLowerCase))
        new File(v)
      else
        throw new Exception("File should be a letter between a - h")
    }
  }

  final case class Position(file: File, rank: Rank) {
    override def toString: String = s"$file$rank"
  }
  object Position {

    implicit val encoder: JsonEncoder[Position] =
      JsonEncoder[String].contramap(_.toString)
    implicit val decoder: JsonDecoder[Position] =
      JsonDecoder[String].map(Position(_))

    def apply(index: Int): Position = {

      // Chess board is 8x8, but I'm starting the board "backwards", with the top of the board at the beginning
      // of the array. The int division is to get the row, and subtracting it from 8 effectively fills in the array
      // backwards so that the higher number ranks are first. Ex. index of 6 gives 8 - (6 % 8) = 8 - 0
      val rank = Rank(8 - (index / 8))

      // Taking the modulus of the index will give its position in the row, and that position will correspond
      // with an index in the list of the letters a, b, c, d, e, f, g, h
      val file = File("abcdefgh".charAt(index % 8).toString)

      Position(file, rank)
    }

    def apply(boardCoords: String): Position = {
      require(boardCoords.length == 2, s"board coordinates must be size 2, received $boardCoords")

      val Array(file, rankString) = boardCoords.toLowerCase.split("")

      require("abcdefgh".contains(file), s"first character of board coords must be letter between a and h, received $file")
      val rank = Try(rankString.toInt) match {
        case Success(i) => i
        case Failure(e) => throw new Exception(s"$e: second character of board coords should be number")
      }
      require(rank > 0 && rank < 9, s"rank should be between 1 and 8, received $rank")

      Position(File(file), Rank(rank))
    }
  }
}

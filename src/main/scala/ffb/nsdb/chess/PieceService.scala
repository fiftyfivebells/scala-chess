package ffb.nsdb.chess

sealed trait PieceService {

  def getType(piece: Piece): PieceType
  def getColor(piece: Piece): Option[Color]
}

trait PieceServiceImpl extends PieceService {

  val TypeMask = 7
  val ColorMask = 8

  def getType(piece: Piece): PieceType = (piece.value & TypeMask) match {
    case 0 => NoPiece
    case 1 => Pawn
    case 2 => Bishop
    case 3 => Knight
    case 4 => Rook
    case 6 => Queen
    case 7 => King
  }

  def getColor(piece: Piece): Option[Color] = (piece.value & TypeMask) match {
    case 0 => None
    case _ => if ((piece.value & ColorMask) == 0) Some(White) else Some(Black)
  }

}

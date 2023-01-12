package ffb.nsdb

package object chess {

  sealed trait Color { def value: Int }
  case object White extends Color { val value = 0 }
  case object Black extends Color { val value = 1 }

  sealed trait PieceType {
    def value: Int
    def mask: Int
  }
  case object Pawn extends PieceType {
    val value = 0
    val mask  = 1
  }
  case object Knight extends PieceType {
    val value = 1
    val mask  = 3
  }
  case object Bishop extends PieceType {
    val value = 2
    val mask  = 2
  }
  case object Rook extends PieceType {
    val value = 3
    val mask  = 4
  }
  case object Queen extends PieceType {
    val value = 4
    val mask  = 6
  }
  case object King extends PieceType {
    val value = 5
    val mask  = 7
  }
  case object NoPiece extends PieceType {
    val value = 6
    val mask  = 0
  }

  case class Piece private (value: Int) extends AnyVal {
    override def toString: String = {

      val pt = (value & 7) match {
        case NoPiece.mask => NoPiece
        case Pawn.mask    => Pawn
        case Knight.mask  => Knight
        case Bishop.mask  => Bishop
        case Rook.mask    => Rook
        case Queen.mask   => Queen
        case King.mask    => King
      }

      pt match {
        case NoPiece => NoPiece.toString
        case t => ((value >> 3) & 1) match {
          case 0 => s"${White.toString} ${t.toString}"
          case 1 => s"${Black.toString} ${t.toString}"
        }
      }
    }
  }
  object Piece {
    def apply(pt: PieceType, c: Option[Color]): Piece = Piece(
        (pt.mask & 7) | (c match {
        case Some(c) => (c.value << 3) & 8
        case None => 0
      })
    )


    def apply(ch: Char, c: Color): Piece = {
      val pt = ch.toLower match {
        case 'p' => Pawn
        case 'n' => Knight
        case 'b' => Bishop
        case 'r' => Rook
        case 'q' => Queen
        case 'k' => King
        case  _  => NoPiece
      }

      Piece(pt, Some(c))
    }
  }
}

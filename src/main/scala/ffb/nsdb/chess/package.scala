package ffb.nsdb


package object chess {

  sealed trait Color
  case object White extends Color
  case object Black extends Color

  sealed trait Piece {
    def materialWeight: Int
  }
  case object Pawn extends Piece {
    val materialWeight: Int = 10
  }
  case object Knight extends Piece {
    val materialWeight: Int = 30
  }
  case object Bishop extends Piece {
    val materialWeight: Int = 35
  }
  case object Rook extends Piece {
    val materialWeight: Int = 50
  }
  case object Queen extends Piece {
    val materialWeight: Int = 80
  }
  case object King extends Piece {
    val materialWeight: Int = 1000
  }
}

package ffb.nsdb.chess

package object board {
  final case class Bitboard(value: Long) extends AnyRef {
    def |(other: Bitboard): Bitboard = Bitboard(value | other.value)

    def |(other: Long): Bitboard = Bitboard(value | other)

    def &(other: Bitboard): Bitboard = Bitboard(value & other.value)

    def &(other: Long): Bitboard = Bitboard(value & other)

    def <<(amt: Int): Bitboard = Bitboard(value << amt)
  }

  final case class CastleRights(rights: Int) extends AnyRef
  final object CastleRights {
    def apply(rightsString: String): CastleRights = {
      CastleRights(
        rightsString.foldLeft(0) { (acc, ch) =>
          ch match {
            case 'K' => acc | 8
            case 'Q' => acc | 4
            case 'k' => acc | 2
            case 'q' => acc | 1
            case '-' => acc | 0
            case  _  => throw new Error("Invalid castle availability provided")
          }
        }
      )
    }
  }

  sealed trait Square {
    def file: String

    def rank: Int

    def position: Int
  }

  final case object A1 extends Square {
    val file = s"a";
    val rank = 1;
    val position = 0
  }
}

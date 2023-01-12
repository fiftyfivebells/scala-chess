package ffb.nsdb.chess

package object move {

  sealed trait MoveType
  final case object Quiet extends MoveType
  final case object Capture extends MoveType
  final case object Castle extends MoveType
  final case object EnPassant extends MoveType
  final case object Promotion extends MoveType
}

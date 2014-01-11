object TTT {
    type Board = Seq[Seq[Option[Player]]]
}
import TTT._

case class Pos(x: Int, y: Int)

sealed trait Player
case object P1 extends Player
case object P2 extends Player

sealed trait State { def board: Board }
case class Finished(board: Board, winner: Player) extends State
case class InPlay(board: Board, player: Player)  extends State
object NewBoard extends InPlay(Seq.fill(3)(Seq.fill(3)(None)), P1)


object TicTacToe {
    def move(gs: InPlay, pos: Pos): State = {
       def update(board: Board, pos: Pos) = board.updated(pos.x, board(pos.x).updated(pos.y, Some(gs.player)))

       def isFinished(board: Board) = {
            def samePlayer(moves: Seq[Option[Player]]) = moves.flatten.size == 3 && moves.distinct.size == 1

            Seq(board(0), board(1), board(2),
                board.map(_(0)), board.map(_(1)), board.map(_(2)),
                (0 to 2).map(i => board(i)(i)), (0 to 2).map(i => board(i)(2-i)))
                .exists(samePlayer)
       }

       val switchPlayer = Map[Player, Player](P1 -> P2, P2 -> P1)

       val newBoard = update(gs.board, pos)

       if (isFinished(newBoard)) Finished(newBoard, gs.player)
       else InPlay(newBoard, switchPlayer(gs.player))
    }

    def whoseTurn(gs: InPlay) = gs.player

    def whoWon(gs: Finished) = gs.winner

    def playerAt(gs: State, pos: Pos) = gs.board(pos.x)(pos.y)

    def printGS(gs: State) = { for (row <- gs.board) yield { for (square <- row) yield print(f"$square%8s "); println}; println }

    def pm(gs: InPlay, pos: Pos) = {val s = move(gs, pos); printGS(s); s}

    def md(gs: State, pos: Pos) = gs match {
        case ip@InPlay(b, p) => pm(ip, pos)
        case f@Finished(b, w) => throw new Exception(s"Player ${whoWon(f)} Won!")
    }

    def main(args: Array[String]) = {
        printGS(NewBoard)

        val gs1 = md(NewBoard, Pos(1, 1))
        val gs2 = md(gs1, Pos(0, 0))
        val gs3 = md(gs2, Pos(1, 2))
        val gs4 = md(gs3, Pos(0, 1))
        val gs5 = md(gs4, Pos(1, 0))
        val gs6 = md(gs5, Pos(2, 2))
    }

}

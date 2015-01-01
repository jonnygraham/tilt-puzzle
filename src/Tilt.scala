import scala.collection.immutable

/**
 * Created by jonnyg on 30/12/2014.
 */
sealed trait BoardCell
sealed trait Piece extends BoardCell {
  def x:Int
  def y:Int
}

sealed trait MoveablePiece extends Piece {
  def copy(x: Int, y :Int) : MoveablePiece
}
case class GoodPiece(x: Int, y :Int) extends MoveablePiece {
  def copy(x: Int, y :Int) = GoodPiece(x,y)
}
case class BadPiece(x: Int, y :Int) extends MoveablePiece {
  def copy(x: Int, y :Int) = BadPiece(x,y)
}
case class Block(x: Int, y : Int) extends Piece
case object Hole extends BoardCell
case object Space extends BoardCell


object Tilt {

  def movePiece2[P <: MoveablePiece](piece : P, xOffset : Int = 0, yOffset : Int = 0): P = {
    piece match {
      case GoodPiece(x,y) => GoodPiece(x + xOffset, y + yOffset)
      case BadPiece(x,y) => BadPiece(x + xOffset, y + yOffset)
    }
  }.asInstanceOf[P]

  def movePiece(piece : MoveablePiece, xOffset : Int = 0, yOffset : Int = 0): MoveablePiece = piece.copy(piece.x + xOffset, piece.y + yOffset)

  /*
  def movePiece(piece : GoodPiece, xOffset : Int = 0, yOffset : Int = 0) =  GoodPiece(piece.x + xOffset, piece.y + yOffset)
  def movePiece(piece : BadPiece, xOffset : Int = 0, yOffset : Int = 0) =  BadPiece(piece.x + xOffset, piece.y + yOffset)
    */

  case class Board(pieces : List[Piece],width : Int, height : Int) {
    val holePos = ((height-1)/2,(width-1)/2)
    def findPieceAt(x : Int, y : Int) = if (x==holePos._1 && y==holePos._2) Hole else pieces.find(p=>p.x==x && p.y==y).getOrElse(Space)
    lazy val cols = (0 to width-1).map{c=> (0 to height-1).map { r=> findPieceAt(c,r)}.toList } .toList
    lazy val rows = (0 to height-1).map {r=> (0 to width-1).map { c=> findPieceAt(c,r)}.toList }.toList

    override def toString() = {
      rows.map{r=>r.map(_ match {
        case p: GoodPiece => "+"
        case p : BadPiece => "X"
        case p: Block => "#"
        case Hole => "O"
        case Space => "."
      }).mkString}.mkString("\n")
    }
  }

  def isSolved(b : Board) = b.pieces.forall{
    case GoodPiece(x,y) => x == b.holePos._1 && y == b.holePos._2
    case BadPiece(x,y) => !(x == b.holePos._1 && y == b.holePos._2)
    case _ => true
  }

  def isFailed(b : Board) = b.pieces.exists{
    case BadPiece(x,y) => x == b.holePos._1 && y == b.holePos._2
    case _ => false
  }

  sealed trait Dir
  case object Start extends Dir
  case object Up extends Dir
  case object Down extends Dir
  case object Left extends Dir
  case object Right extends Dir

  def calcDistance(cellsInDir : List[BoardCell]) : Int = cellsInDir match {
    case Nil => 0
    case Hole :: rest => 1
    case Space :: rest => 1 + calcDistance(rest)
    case Block(_,_) :: rest => 0
    case (p : Piece) :: rest => calcDistance(rest)
  }
  def move(board : Board, dir : Dir) = {
    val newPieces = {
      val moveablePieces = board.pieces.collect{ case p : MoveablePiece if !(p.x == board.holePos._1 && p.y == board.holePos._2) => p}
      val fixedPieces = board.pieces.filterNot(p => moveablePieces.contains(p))
      fixedPieces ::: {
        dir match {
          case Up =>
            moveablePieces.map { p =>
              val cellsInDir = board.cols(p.x).take(p.y).reverse
              val distance = calcDistance(cellsInDir)
              movePiece(p, 0, -distance)
            }
          case Down =>
            moveablePieces.map { p =>
              val cellsInDir = board.cols(p.x).drop(1 + p.y)
              val distance = calcDistance(cellsInDir)
              movePiece(p, 0, distance)
            }
          case Left =>
            moveablePieces.map { p =>
              val cellsInDir = board.rows(p.y).take(p.x).reverse
              val distance = calcDistance(cellsInDir)
              movePiece(p, -distance, 0)
            }
          case Right =>
            moveablePieces.map { p =>
              val cellsInDir = board.rows(p.y).drop(1 + p.x)
              val distance = calcDistance(cellsInDir)
              movePiece(p, distance, 0)
            }
        }
      }
    }
    Board(newPieces, board.width, board.height)
  }

  val dirs : List[Dir]= List(Up,Down,Left,Right)

  def findSolution(startBoard : Board) = {
    def findSolution0(boards: List[(Board, List[(Dir, Board)])]): List[Dir] = {
//      println("iter")
      val validBoards = boards.flatMap { case (board, history) =>
        if (isFailed(board)) None
        else if (isSolved(board)) Some(board, history, true)
        else Some(board, history, false)
      }
      if (validBoards.size == 0) Nil
      else {
        val sol = validBoards.collectFirst { case (_, history, true) => history.map(_._1).reverse.tail}
        sol.getOrElse {
          val nextBoards = validBoards.flatMap { case (board, history, _) =>
            val nextBoards = dirs.flatMap { dir =>
              val nextBoard = move(board, dir)
              if (history.map(_._2).contains(nextBoard)) None
              else Some(nextBoard, (dir, nextBoard) :: history)
            }
            nextBoards
          }
          findSolution0(nextBoards)

        }
      }
    }
    val init = List((startBoard, List((Start,startBoard))))
    findSolution0(init)
  }

  def findSolution2(startBoard: Board) = {
    def findSolution0(boards: List[(Board, List[Dir])], visited: Set[Board]): List[Dir] = {
      //      println("iter")
      boards match {
        case Nil => Nil
        case boards =>
          val nextBoards = boards.flatMap { case (board, path) =>
            dirs.filter(d => path.headOption.map(lastD => d != lastD).getOrElse(true)).flatMap { dir =>
              val nextBoard = move(board, dir)
              if (visited.contains(nextBoard) || isFailed(nextBoard)) None
              else Some(nextBoard, dir :: path)
            }
          }
          val solution = nextBoards.collectFirst { case (b, path) if isSolved(b) => path}
          solution match {
            case Some(path) => path
            case None =>
              val newVisited = nextBoards.map(_._1).toSet ++ visited
              findSolution0(nextBoards, newVisited)
          }
      }
    }
    val init = List((startBoard, Nil))
    findSolution0(init, Set(startBoard))
  }
}
object Test extends  App {
  import Tilt._

  def solve(b : Board) = {
    println(b)
    val sol = findSolution2(b)
    println(s"Solution in ${sol.length} moves: $sol")
  }
  solve(Board(List(GoodPiece(0,2),GoodPiece(1,2)),5,5))
  solve(Board(List(GoodPiece(0,2),Block(3,0)),5,5))


  solve(Board(List(GoodPiece(0,0),Block(0,3),Block(3,0)),5,5))
  solve(Board(List(GoodPiece(0,0),BadPiece(0,1),Block(0,3),Block(3,0)),5,5))
  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0)),5,5))
  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0),Block(1,3)),5,5))
  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0),GoodPiece(1,2)),5,5))

  solve(Board(List(BadPiece(0,0),GoodPiece(1,0),Block(2,0),BadPiece(0,4),GoodPiece(1,4)),5,5))
  solve(Board(List(Block(0,0),Block(2,0),GoodPiece(3,0),GoodPiece(4,0),BadPiece(3,1),Block(4,1),Block(3,2),Block(2,3)),5,5))
}

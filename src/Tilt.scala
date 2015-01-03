import scala.collection.immutable

/**
 * Created by jonnyg on 30/12/2014.
 */
sealed trait BoardCell
sealed trait Piece extends BoardCell {
  def x:Int
  def y:Int
}

sealed trait MoveablePiece[P] extends Piece {
  def move(x: Int = x, y :Int = y) : MoveablePiece[P]
}
case class GoodPiece(x: Int, y :Int) extends MoveablePiece[GoodPiece] {
  def move(x: Int = x, y :Int = x) = copy(x,y)
}
case class BadPiece(x: Int, y :Int) extends MoveablePiece[BadPiece] {
  def move(x: Int = x, y :Int = x) = copy(x,y)
}
case class Block(x: Int, y : Int) extends Piece
case object Hole extends BoardCell
case object Space extends BoardCell


object Tilt {

  def movePiece[P](piece : MoveablePiece[P], xOffset : Int = 0, yOffset : Int = 0) = piece.move(piece.x + xOffset, piece.y + yOffset)

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

  def calcDistance(cellsInDir : List[BoardCell]) : Int = {
    def calcDistance0(cellsInDir : List[BoardCell],piecesInTheWay : Int, distance : Int) : Int =
    cellsInDir match {
      case Nil => distance
      case Hole :: rest => piecesInTheWay + distance + 1
      case Space :: rest => calcDistance0(rest,piecesInTheWay,distance+1)
      case Block(_,_) :: rest => distance
      case (p : Piece) :: rest => calcDistance0(rest,piecesInTheWay+1,distance)
    }
    calcDistance0(cellsInDir,0,0)
  }
  def move(board : Board, dir : Dir) = {
    val newPieces = {
      val moveablePieces = board.pieces.collect{ case p : MoveablePiece[_] if !(p.x == board.holePos._1 && p.y == board.holePos._2) => p}
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

  def findSolution(startBoard: Board) = {
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
            case Some(path) => path.reverse
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
    val sol = findSolution(b)
    if (sol.length > 0) println(s"Solution in ${sol.length} moves: $sol")
    else println("Cannot be solved!")
  }
//  solve(Board(List(GoodPiece(0,2),GoodPiece(1,2)),5,5))
//  solve(Board(List(GoodPiece(0,2),Block(3,0)),5,5))
//
//
//  solve(Board(List(GoodPiece(0,0),Block(0,3),Block(3,0)),5,5))
//  solve(Board(List(GoodPiece(0,0),BadPiece(0,1),Block(0,3),Block(3,0)),5,5))
//  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0)),5,5))
//  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0),Block(1,3)),5,5))
//  solve(Board(List(GoodPiece(0,0),BadPiece(2,0),BadPiece(0,1),Block(0,3),Block(3,0),GoodPiece(1,2)),5,5))
//
//  solve(Board(List(BadPiece(0,0),GoodPiece(1,0),Block(2,0),BadPiece(0,4),GoodPiece(1,4)),5,5))
//  solve(Board(List(Block(0,0),Block(2,0),GoodPiece(3,0),GoodPiece(4,0),BadPiece(3,1),Block(4,1),Block(3,2),Block(2,3)),5,5))
//
  solve(Board(List(Block(0,0),Block(2,1),Block(3,2),Block(2,3),Block(1,4),GoodPiece(4,0),GoodPiece(0,1),BadPiece(0,2),BadPiece(4,1)),5,5))

  solve(Board(List(Block(4,0),Block(3,1),GoodPiece(4,1),BadPiece(3,0)),5,5))

  solve(Board(List(Block(2,0),Block(1,1),Block(3,3),Block(2,4),GoodPiece(4,0),GoodPiece(0,4)),5,5))

  solve(Board(List(Block(2,1),Block(1,2),Block(3,2),Block(2,4),GoodPiece(2,0)),5,5))
}

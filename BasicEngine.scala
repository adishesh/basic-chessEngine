import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object BasicEngine {

  val chessBoard =Array(Array("r","k","b","q","a","b","k","r"),
                        Array("p","p","p","p","p","p","p","p"),
                        Array(" "," "," "," "," "," "," "," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array("P","P","P","P","P","P","P","P"),
                        Array("R","K","b","Q","A","b","K","R"));

  var whiteKing = 0
  var blackKing = 0

  def main(args: Array[String]) {
   println(possibleKnight(4,0))
  }

  def getPossibleMoves(): String = {
    val movesList = ListBuffer()
    for( i <- (0 until 8).view;j <- (0 until 8).view) {
      chessBoard(i)(j) match {
        case "P" => possiblePawn(i,j)
        case "K" => possibleKnight(i,j)
        case "R" => possibleRook(i,j)
        case "B" => possibleBishop(i,j)
        case "Q" => possibleQueen(i,j)
        case "A" => possibleKing(i,j)
      }
    }
   "test"
  }

  def checkSquare(x:Int, y:Int):String = {
     if( Character.isLowerCase(chessBoard(x)(y).charAt(0)) ) "occupied"
     else if( chessBoard(x)(y).equals(" ") ) "empty"
     else "self"
  }

  def checkLinearSquares(i:Int, j:Int, getNextVal:(Int,Int) => (Int,Int)): List[(Int,Int)]  = {

       val movesList = ListBuffer[(Int,Int)]()
       val (x,y) = getNextVal(i,j)

       if( 0 <= x && x < 8 && 0 <= y && y  < 8 ) {

         val squareStatus = checkSquare(x,y)

         // if you hit an enemy piece take it and end the move
         if(squareStatus.equals("occupied")){
           movesList.append((x,y))
         }
         // if it is an empty square, check if it is a valid move and recurse
         else if( squareStatus.equals("empty") ){
            movesList.append((x,y))
            movesList++= checkLinearSquares(x,y,getNextVal)
         }

       }

       movesList.toList
  }

  def possiblePawn(i:Int, j:Int) :String = {""}

  def possibleKnight(i:Int, j:Int):List[(Int,Int)] =
  {
    val moves = ListBuffer[(Int,Int)]()

    for( k <- (-1 to 1 by 2); l <- (-1 to 1 by 2) ) {

      if( i+2*k >= 0 && i+2*k < 8 && j+l >= 0 && j+l < 8 ) {
        if (!checkSquare(i+2*k,j+l).equals("self") && !isKingInCheck(i+2*k,j+l)) {
           moves.append((i+2*k,j+l))
        }
      }

      if( i+l >= 0 && i+l < 8 && j+2*k >= 0 && j+2*k < 8 ) {
        if (!checkSquare(i+l,j+2*k).equals("self") && !isKingInCheck(i+l,j+2*k)) {
           moves.append((i+l,j+2*k))
        }
      }

    }
    moves.toList
  }

  def possibleRook(i:Int,j:Int):List[(Int,Int)] =
  {
    val moves = ListBuffer[(Int,Int)]()

    //down
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x+1,y))
    //up
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x-1,y))   
    //left
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x,y-1))   
    //right
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x,y+1))   
   
    moves.toList
  }

  def possibleBishop(i:Int,j:Int):List[(Int,Int)] =
  {
    var moves = ListBuffer[(Int,Int)]()

    //down right
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x+1,y+1))   
    //up right
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x+1,y-1))   
    //down left
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x-1,y+1))   
    //up left
    moves++= checkLinearSquares(i,j,(x:Int,y:Int) => (x-1,y-1))   
   
    moves.toList
  }

  def possibleQueen(i:Int, j:Int):List[(Int,Int)] =
  {
    possibleRook(i,j):::possibleBishop(i,j)
  }

  def possibleKing(i:Int,j:Int):List[(Int,Int)] =
  {
    val list = ListBuffer[(Int,Int)]();

    // get the first empty square around the king
    for( x <- (-1 to 1);y <- (-1 to 1)) {
      if (i != -1 && j != -1) {
        if(!checkSquare(i+x,j+y).equals("self") && !isKingInCheck(i+x,j+y)){
             list.append((i + x, j + y))
        }
      }
    }

    list.toList
  }

  def isKingInCheck(i:Int, j:Int):Boolean =
  {
    false
  }


}



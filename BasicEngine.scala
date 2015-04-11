import scala.collection.mutable.ListBuffer

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
    println(possiblePawn(6,1))
  }

  def getPossibleMoves(): String = {

    var king = (-1,-1)
    val allPieceMoves = ListBuffer[((Int,Int),List[(Int,Int)])]()

    for( i <- (0 until 8).view;j <- (0 until 8).view) {

      val pieceMoves = chessBoard(i)(j) match {
                            case "P" => {
                              possiblePawn(i,j)
                            }
                            case "K" => possibleKnight(i,j)
                            case "R" => possibleRook(i,j)
                            case "B" => possibleBishop(i,j)
                            case "Q" => possibleQueen(i,j)

                            case "A" => {
                              king = (i,j);
                              possibleKing(i,j)
                            }
                            case _ => Nil
                       }

      if(pieceMoves != Nil){
        allPieceMoves.append( ((i,j),pieceMoves))
      }
    }

    //filter out moves in which the king is in check
    allPieceMoves map ()

  }

  def checkSquare(x:Int, y:Int):String = {
    if( Character.isLowerCase(chessBoard(x)(y).charAt(0)) ) "occupied"
    else if( chessBoard(x)(y).equals(" ") ) "empty"
    else "self"
  }

  def lineOfSight(i:Int, j:Int, getNextVal:(Int,Int) => (Int,Int)) :List[(Int,Int)] =  {

    def inner(i:Int, j:Int, getNextVal:(Int,Int) => (Int,Int), moves: ListBuffer[(Int,Int)]): ListBuffer[(Int,Int)]  = {

      val (x, y) = getNextVal(i, j)

      if (0 <= x && x < 8 && 0 <= y && y < 8) {

        val squareStatus = checkSquare(x, y)

        // if you hit an enemy piece take it and end the move
        if (squareStatus.equals("occupied")) {
            moves.append((x, y))
            return moves
        }
          
        // if it is an empty square, check if it is a valid move and recurse
        if (squareStatus.equals("empty")) {
            moves.append((x, y))
            return inner(x, y, getNextVal,moves)
        }
      }
      
      return moves
    }

    inner(i,j,getNextVal,ListBuffer[(Int,Int)]()).toList
  }

  def possiblePawn(i:Int, j:Int) :List[(Int,Int)] = {
    //TODO en passant

    val moves = ListBuffer[(Int,Int)]()
    //move one up
    if (checkSquare(i-1,j).equals("empty")){
      moves.append((i-1,j))
    }
    //capture left
    if( j-1 >= 0) {
      if (checkSquare(i - 1, j-1).equals("occupied")) {
        moves.append((i - 1, j-1))
      }
    }
    //capture right
    if(j+1 < 8) {
      if (checkSquare(i - 1, j+1).equals("occupied")) {
        moves.append((i - 1, j+1))
      }
    }
    //move two up
    if( i == 6 ) {
      if (checkSquare(4, j).equals("empty")) {
        moves.append((4, j))
      }
    }
    moves.toList
  }

  def possibleKnight(i:Int, j:Int):List[(Int,Int)] =
  {
    val moves = ListBuffer[(Int,Int)]()

    for( k <- (-1 to 1 by 2); l <- (-1 to 1 by 2) ) {

      if( i+2*k >= 0 && i+2*k < 8 && j+l >= 0 && j+l < 8 ) {
        if (!checkSquare(i+2*k,j+l).equals("self")) {
          moves.append((i+2*k,j+l))
        }
      }

      if( i+l >= 0 && i+l < 8 && j+2*k >= 0 && j+2*k < 8 ) {
        if (!checkSquare(i+l,j+2*k).equals("self")) {
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
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x+1,y))
    //up
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x-1,y))
    //left
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x,y-1))
    //right
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x,y+1))

    moves.toList
  }

  def possibleBishop(i:Int,j:Int):List[(Int,Int)] =
  {
    var moves = ListBuffer[(Int,Int)]()

    //down right
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x+1,y+1))
    //up right
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x+1,y-1))
    //down left
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x-1,y+1))
    //up left
    moves++= lineOfSight(i,j,(x:Int,y:Int) => (x-1,y-1))

    moves.toList
  }

  def possibleQueen(i:Int, j:Int):List[(Int,Int)] =
  {
    possibleRook(i,j):::possibleBishop(i,j)
  }

  def possibleKing(i:Int,j:Int):List[(Int,Int)] =
  { //TODO castling
    val list = ListBuffer[(Int,Int)]();

    // get the first empty square around the king
    for( x <- (-1 to 1);y <- (-1 to 1)) {
      if (i != -1 && j != -1) {
        if(!checkSquare(i+x,j+y).equals("self")){
          list.append((i + x, j + y))
        }
      }
    }

    list.toList
  }

  def isKingInCheck(i:Int, j:Int):Boolean =
  {
    //check if king is in the line of sight of a bishop or rook or a queen
    val star = possibleQueen(i,j) map ( x => chessBoard(x._1)(x._2)) filter( x => x.equals("q") ||  x.equals("b") || x.equals("r") ) length;
    if(star > 0){
      return false
    }

    //check if the position has the enemy king as a neighbor
    val neighbors = possibleKing(i,j) map ( x => chessBoard(x._1)(x._2)) filter( _.equals("a") ) length;
    if(neighbors > 0){
      return false
    }

    //check for pawns
    if(i > 0){
      if( j > 0 && chessBoard(i-1)(j-1).equals("p")){
        return false
      }
      if( j < 7 && chessBoard(i-1)(j+1).equals("p")){
        return false
      }
    }

    // chess knight moves are 'commutative' so we can safely reuse posssibleKnights() to get attacking knights
    //check for knight attacks
    val knights = possibleKnight(i,j) map ( x => chessBoard(x._1)(x._2)) filter( _.equals("k") ) length;
    if(knights > 0){
      return false
    }

    return true
  }


}
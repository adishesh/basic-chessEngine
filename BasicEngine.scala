import scala.collection.mutable.ListBuffer

object BasicEngine {

  var chessBoard =Array(
    Array(" "," ","r"," ","a"," "," "," "),
    Array(" ","P"," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," ","q"," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," ","A"));

  val chessBoard1 =Array(Array("r","k","b","q","a","b","k","r"),
    Array("p","p","p","p","p","p","p","p"),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array("P","P","P","P","P","P","P","P"),
    Array("R","K","b","Q","A","b","K","R"));

  val boardCopy = Array(
    Array(" "," ","r"," ","a"," "," "," "),
    Array(" ","P"," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," ","q"," "),
    Array(" "," "," "," "," "," "," "," "),
    Array(" "," "," "," "," "," "," ","A"));

  var king = (-1,-1)

  def getBoardWeight():Int = {
    0
  }

  def main(args: Array[String]) {
    val moves = getPossibleMoves()
    moves foreach ( x => {
         makeMove(x._1,x._2,x._3)
         printBoard()
         for( i <- (0 until 8); j <- (0 until 8)){
           chessBoard(i)(j) = boardCopy(i)(j)
         }
       }
     )
    println(moves)

   }

  def alphaBeta(depth: Int, alpha: Int, beta: Int, move :((Int,Int),(Int,Int),String), player :Boolean) ={
    val possibleMoves = getPossibleMoves()
    if(depth == 0 || possibleMoves.length == 0){
      return (move,getBoardWeight()*(if(player)-1 else 1))
    }
    //player = !player

  }

  def flipBoard() ={

  }

  def getPossibleMoves(): ListBuffer[((Int,Int),(Int,Int),String)] = {

    val allPieceMoves = ListBuffer[((Int,Int),(Int,Int),String)]()

    for( i <- (0 until 8).view;j <- (0 until 8).view) {

      val pieceMoves = chessBoard(i)(j) match {
                            case "K" => possibleKnight(i,j) map ( x => (x._1,x._2,null))
                            case "R" => possibleRook(i,j) map ( x => (x._1,x._2,null))
                            case "B" => possibleBishop(i,j) map ( x => (x._1,x._2,null))
                            case "Q" => possibleQueen(i,j) map ( x => (x._1,x._2,null))
                            case "P" => possiblePawn(i,j)
                            case "A" => {
                              // set the king position here instead of doing this
                              // separately in another loop
                              king = (i,j);
                              possibleKing(i,j)
                            }
                            case _ => Nil
                       }

      if(pieceMoves != Nil){
        allPieceMoves++= (pieceMoves map (x => ((i,j),(x._1,x._2),x._3)))
      }
    }

    // filter out moves in which the king is in check
    // this can be done only when the above loop completes as
    // the king position needs to be set
    allPieceMoves filter (x => isAValidMove(x._1,x._2,x._3))
  }

  def isAValidMove(from: (Int,Int), to: (Int,Int), notation: String) = {

    val prevKing = king
    if(chessBoard(from._1)(from._2).equals("A")){
      king = to
    }

    makeMove(from,to,notation)

    val isInCheck = isKingInCheck()

    // reset board
    for( i <- (0 until 8); j <- (0 until 8)){
      chessBoard(i)(j) = boardCopy(i)(j)
    }
    // skip the king move check, it's faster this way
    king = prevKing

    !isInCheck
  }

  def makeMove(from: (Int,Int), to: (Int,Int), notation: String ) = {

    // TODO handle castling and enpassant
    val destPiece =  notation match {
      case null => chessBoard(from._1)(from._2)
      case _ => notation
    }
    chessBoard(to._1)(to._2) = destPiece
    chessBoard(from._1)(from._2) = " "
  }

  def printBoard(): Unit ={
    chessBoard foreach( y =>{ y foreach (x => print(x+"|") ); println("\n----------------")} )
    println
  }

  def checkSquare(x:Int, y:Int):String = {
    if( Character.isLowerCase(chessBoard(x)(y).charAt(0)) ) "occupied"
    else if( chessBoard(x)(y).equals(" ") ) "empty"
    else "self"
  }

  def lineOfSight(i:Int, j:Int, getNextVal:(Int,Int) => (Int,Int)) :ListBuffer[(Int,Int)] =  {

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

    inner(i,j,getNextVal,ListBuffer[(Int,Int)]())
  }

  def possiblePawn(i:Int, j:Int) :ListBuffer[(Int,Int,String)] = {

    //TODO en passant

    val moves = ListBuffer[(Int,Int,String)]()
    //move one up
    if (checkSquare(i-1,j).equals("empty")){
      moves.append((i-1,j, null))
    }
    //capture left
    if( j-1 >= 0) {
      if (checkSquare(i - 1, j-1).equals("occupied")) {
        moves.append((i - 1, j-1, null))
      }
    }
    //capture right
    if(j+1 < 8) {
      if (checkSquare(i - 1, j+1).equals("occupied")) {
        moves.append((i - 1, j+1, null))
      }
    }
    //move two up
    if( i == 6 ) {
      if (checkSquare(4, j).equals("empty")) {
        moves.append((4, j, null))
      }
    }

    // pawn promotion
    if( i == 1){
      val promoteList = ListBuffer[(Int,Int,String)]()
      for( i <- List("Q","R","B","K")){
        promoteList ++= moves map ( x => (x._1,x._2,i))
      }
      return promoteList
    }

    moves
  }

  def possibleKnight(i:Int, j:Int):ListBuffer[(Int,Int)] =
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
    moves
  }

  def possibleRook(i:Int,j:Int):ListBuffer[(Int,Int)] =
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

    moves
  }

  def possibleBishop(i:Int,j:Int):ListBuffer[(Int,Int)] =
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

    moves
  }

  def possibleQueen(i:Int, j:Int):ListBuffer[(Int,Int)] =
  {
    possibleRook(i,j)++possibleBishop(i,j)
  }

  def possibleKing(i:Int,j:Int):ListBuffer[(Int,Int,String)] =
  { //TODO castling
    val moves = ListBuffer[(Int,Int,String)]();

    // get the first empty square around the king
    for( x <- (-1 to 1);y <- (-1 to 1)) {
      if (( !(x == 0 && y == 0 ) && i+x  >= 0 && i+x < 8 && j+y >= 0 && j+y < 8)) {
        if(!checkSquare(i+x,j+y).equals("self")){
          moves.append((i + x, j + y, null))
        }
      }
    }

    moves
  }

  def isKingInCheck():Boolean =
  {
    val (i,j) = king

    //check if king is in the line of sight of a bishop or a rook or a queen
    val star = possibleQueen(i,j) map ( x => chessBoard(x._1)(x._2)) filter( x => x.equals("q") ||  x.equals("b") || x.equals("r") ) length;
    if(star > 0){
      return true
    }

    //check if the position has the enemy king as a neighbor
    val neighbors = possibleKing(i,j) map ( x => chessBoard(x._1)(x._2)) filter( _.equals("a") ) length;
    if(neighbors > 0){
      return true
    }

    //check for pawns
    if(i > 0){
      if( j > 0 && chessBoard(i-1)(j-1).equals("p")){
        return true
      }
      if( j < 7 && chessBoard(i-1)(j+1).equals("p")){
        return true
      }
    }

    // chess knight moves are 'commutative' so we can safely reuse posssibleKnights() to get attacking knights
    //check for knight attacks
    val knights = possibleKnight(i,j) map ( x => chessBoard(x._1)(x._2)) filter( _.equals("k") ) length;
    if(knights > 0){
      return true
    }

    return false
  }


}
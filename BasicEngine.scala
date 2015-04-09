import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

object BasicEngine {

  val chessBoard =Array(Array("r","k","b","q","a","b","k","r"),
                        Array("p","p","p","p","p","p","p","p"),
                        Array(" "," "," "," "," "," ","P"," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array(" "," "," "," "," "," "," "," "),
                        Array("P","P","P","P","P","P","P","P"),
                        Array("R","K","B","Q","A","B","K","R"));

  var whiteKing = 0
  var blackKing = 0

  def main(args: Array[String]) {

  }

  def getPossibleMoves(): String = {
    val movesList = ListBuffer()
    for( i <- (0 until 8).view;j <- (0 until 8).view) {
      chessBoard(i)(j) match {
        case "P" => possiblePawn(i)
        case "K" => possibleKnight(i)
        case "R" => possibleRook(i)
        case "B" => possibleBishop(i)
        case "Q" => possibleQueen(i)
        case "A" => possibleKing(i,j)
      }
    }
  }

  def possiblePawn(i:Int) :String = {}
  def possibleKnight(i:Int):String = {}

  def possibleRook(i:Int,j:Int):List[(Int,Int)] = {
    val list = ListBuffer[(Int,Int)]()

    var k = 1;
    while(i+k < 8)
    {
      if( chessBoard(i+k)(j).equals(" ") )
      {
        chessBoard(i)(j) = " "
        chessBoard(i+k)(j) = "R"
        if(isKingSafe())
        {
          list.append((i + k, j))
        }
        chessBoard(i)(j) = "R"
        chessBoard(i+k)(j) = " "
      }

      if( Character.isLowerCase(chessBoard(i+k)(j).charAt(0)) )
      {
        chessBoard(i)(j) = " "
        chessBoard(i+k)(j) = "R"
        if(isKingSafe())
        {
          list.append((i + k, j))
        }
        chessBoard(i)(j) = "R"
        chessBoard(i+k)(j) = " "
        Breaks.break
      }

      k += 1
    }

    k = 1
    while(i-k >= 0)
    {
      if( chessBoard(i-k)(j).equals(" ") )
      {
        chessBoard(i)(j) = " "
        chessBoard(i-k)(j) = "R"
        if(isKingSafe())
        {
          list.append((i + k, j))
        }
        chessBoard(i)(j) = "R"
        chessBoard(i-k)(j) = " "
      }

      if( Character.isLowerCase(chessBoard(i-k)(j).charAt(0)) )
      {
        chessBoard(i)(j) = " "
        chessBoard(i-k)(j) = "R"
        if(isKingSafe())
        {
          list.append((i + k, j))
        }
        chessBoard(i)(j) = "R"
        chessBoard(i-k)(j) = " "
        Breaks.break
      }

      k -= 1
    }
  }
  def possibleBishop(i:Int):String = {}
  def possibleQueen(i:Int):String = {}

  def possibleKing(i:Int,j:Int):List[(Int,Int)] = {
    val list = ListBuffer[(Int,Int)]();
    // get the first empty square around the king
    for( x <- (-1 to 1);y <- (-1 to 1))
      if ( i != j != -1 )
        if( (Character.isLowerCase(chessBoard(i+x)(j+y).charAt(0)) || chessBoard(i+x)(j+y).equals(" ")))
        {
           chessBoard(i)(j) = " "
           chessBoard(i+x)(j+y) = "A"
           if(isKingSafe())
           {
             list.append((i + x, j + y))
           }
           chessBoard(i)(j) = "A"
           chessBoard(i+x)(j+y) = " "
        }
    list.toList
  }

  def isKingSafe() = {
    true
  }


}

import scala.annotation.tailrec
import scala.util.Random
import Knight.{KnightPos, getGoal}

object Main {

  var count: Int = 0
  var start: KnightPos =_
  var goal: KnightPos =_

  private def isRangeBoard(f: String, r: String): Boolean ={
    val l = List("1","2","3","4","5","6","7","8")
    l.contains(f) && l.contains(r)
  }

  private def isNum(n: String): Either[String, Int] ={
    try Right(n.toInt)
    catch { case e: Exception => Left(e.getMessage) }
  }

  @tailrec
  def getMoveCount: Int = {
    val c = io.StdIn.readLine
    val eC: Either[String, Int] = isNum(c)
    eC match {
      case Right(x) => x
      case Left(e) =>
        println(s"The value entered is invalid \n by $e")
        getMoveCount
    }
  }

  @tailrec
  private def inputKnight: KnightPos ={
    println("file: ")
    val file = io.StdIn.readLine
    println("rank: ")
    val rank = io.StdIn.readLine
    if(isRangeBoard(file, rank)) KnightPos(file.toInt, rank.toInt)
    else {
      println("Please enter a value from 1 to 8.")
      inputKnight
    }
  }

  @tailrec
  def getReallyMoveKnight(bfr: Option[KnightPos]): KnightPos ={
    val k: KnightPos = inputKnight
    if(!Knight.isReallyMove(bfr, k)) getReallyMoveKnight(bfr)
    else k
  }

  @tailrec
  def appRoutine(bfr: Option[KnightPos], n: Int): KnightPos ={
    println("\n*****************************************************")
    bfr match {
      case Some(x) => println("\nNow Position is File: %s, Rank: %s.".format(x.files, x.ranks))
      case None => println("Now Position is File: %s, Rank: %s.".format(start.files, start.ranks))
    }
    println(s"Now Count is $n/$count")
    println(s"Your Goal is File: ${goal.files}, Rank: ${goal.ranks}")
    println("*****************************************************")
    val k: KnightPos = getReallyMoveKnight(bfr)
    if(n >= count) k
    else {
      appRoutine(Some(k), n + 1)
    }
  }

  def main(args: Array[String]): Unit = {

    println("*****************************************************")
    println("**************** Welcome Knight Tour ****************")
    println("*****************************************************")
    println("\nPlease enter the number of times the knight moves.")
    val c = getMoveCount
    count = c
    println("\nOK!")
    println("\nPlease enter the start position of the night.")
    start = inputKnight
    Knight.used = Seq(start)
    println(s"\nOK! Start position is File: ${start.files}, Rank: ${start.ranks}.")

    val goalList = getGoal(start, count)
    val rdmAry = Random.nextInt(goalList.length -1)
    goal = goalList(rdmAry)
    println(s"\nYour goal is File: ${goal.files}, Rank: ${goal.ranks} !")
    println("Let's Start !")
    val userGoal: KnightPos = appRoutine(None, 1)

    println("\n*******************************************************\n")
    if(userGoal == goal) println("Clear!!!")
    else println("False...")
    println("\n*******************************************************")
  }

}

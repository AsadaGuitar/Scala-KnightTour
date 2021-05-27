import Knight.{KnightPos, knightMove, nextMove}

object Main {

  def ablePos(kl: Seq[KnightPos]): Seq[KnightPos] ={
    kl.filter(x => x.files != 0 && x.ranks != 0)
  }

  def sortKnights(kl: Seq[KnightPos]): Seq[KnightPos] ={
    kl.sortBy(x => x.ranks).sortBy(x => x.files)
  }

  def main(args: Array[String]): Unit = {

    val knightSort = (x: Seq[KnightPos]) => sortKnights(ablePos(x)).distinct
    val samePosList = (x: Seq[KnightPos], y: KnightPos) => x.filter(x => x == y)

    val k = KnightPos(6,2)
    val km1 = knightMove(k)
    val km2 = nextMove(km1)
    val km3 = nextMove(km2)
    val km4 = nextMove(km3)

    println(knightSort(km4)) //4回動いたポジション
    val select = io.StdIn.readLine

    val selectPosList: Seq[(KnightPos)] = samePosList(km4, knightSort(km4)(select.toInt))
    println("select Position is " + knightSort(selectPosList))

    val beforeKm4 = Knight.beforeKnightList(selectPosList, km3)
    val beforeKm3 = Knight.beforeKnightList(beforeKm4, km2)
    val beforeKm2 = Knight.beforeKnightList(beforeKm3, km1)
    println("start : " + k)
//    println("2: " + knightSort(beforeKm2))
//    println("3: " + knightSort(beforeKm3))
//    println("4: " + knightSort(beforeKm4))
    println("goal : " + knightSort(selectPosList))

    val f1 = io.StdIn.readLine
    val r1 = io.StdIn.readLine
    val mk = KnightPos(f1.toInt, r1.toInt)
    if(knightSort(beforeKm2) contains mk) println("success")
    else println("false")

    val f2 = io.StdIn.readLine
    val r2 = io.StdIn.readLine
    val mk2 = KnightPos(f2.toInt, r2.toInt)
    if(knightSort(beforeKm3) contains mk2) println("success")
    else println("false")

    val f3 = io.StdIn.readLine
    val r3 = io.StdIn.readLine
    val mk3 = KnightPos(f3.toInt, r3.toInt)
    if(knightSort(beforeKm4) contains mk3) println(("success"))
    else println("false")

    val f4 = io.StdIn.readLine
    val r4 = io.StdIn.readLine
    val mk4 = KnightPos(f4.toInt, r4.toInt)
    println(mk4)
  }

}

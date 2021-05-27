object Knight {

  case class KnightPos (files: Int, ranks: Int)

  private def isInBoard(k: KnightPos): Boolean ={
    if ((1 until 9 contains k.files) && (1 until 9 contains k.ranks)) true
    else false
  }

  def knightMove(k: KnightPos): Seq[KnightPos]= {
    for {
      (f, r) <- Seq((2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1))
    } yield {
      val a = KnightPos(k.files + f, k.ranks + r)
      if (isInBoard(a)) a
      else KnightPos(0,0)
    }
  }

  def beforeKnightList(aft: Seq[KnightPos], bfr: Seq[KnightPos]): Seq[KnightPos] ={
    for {
      bk <- bfr
      bmk <- knightMove(bk)
    } yield if(aft contains bmk) bk
    else KnightPos(0,0)
  }

  def nextMove(start: Seq[KnightPos]): Seq[KnightPos] ={
    for{
      k <- start
      m <- knightMove(k)
    } yield m
  }
}

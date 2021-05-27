import scala.annotation.tailrec

object Knight {

  var used: Seq[Knight.KnightPos] =_

  case class KnightPos (files: Int, ranks: Int)

  val sortKnights: Seq[KnightPos] => Seq[KnightPos] = (x: Seq[KnightPos]) => {
    x.distinct.sortBy(x => x.ranks).sortBy(x => x.files)
  }

  def isReallyMove(bfr: Option[KnightPos], aft: KnightPos): Boolean ={
    bfr match {
      case None => true
      case Some(b) =>
        if(!(getAbleMovedNight(b) contains aft)) {
        println("This move is no able.")
        false
      }
      else if (used contains aft) {
        println("You had get this move.")
          false
      }
      else {
        used = used :+ aft
        true
      }
    }
  }

  def getAbleMovedNight(k: KnightPos): Seq[KnightPos] ={
      for {
        x <- optMove(k)
        y <- x
      } yield y
  }

  private def isInBoard(k: KnightPos): Boolean ={
    if ((1 until 9 contains k.files) && (1 until 9 contains k.ranks)) true
    else false
  }

  private def optMove(k: KnightPos): Seq[Option[KnightPos]]= {
    for {
      (f, r) <- Seq((2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1))
    } yield {
      val a = KnightPos(k.files + f, k.ranks + r)
      Some(a).filter(isInBoard)
    }
  }

  def getGoal(k: KnightPos, c: Int): Seq[KnightPos] ={

    def f (kl: Seq[KnightPos]): Seq[KnightPos] ={
      for {
        x <- kl
        y <- getAbleMovedNight(x)
      } yield y
    }

    @tailrec
    def loop (kl: Seq[KnightPos], n: Int): Seq[KnightPos] ={
      if(n >= c) kl
      else {
        val x = f(kl)
        loop(x, n+1)
      }
    }

    sortKnights(loop(getAbleMovedNight(k), 1))
  }
}

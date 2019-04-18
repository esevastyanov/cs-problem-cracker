package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/the-grid-search/problem
  */
object Grid_Search extends App
{
  Solution.main(args)

  object Solution
  {

    def contains(gr: Int, gc: Int, g: Array[Array[Char]], pr: Int, pc: Int, p: Array[Array[Char]]): Boolean = {
      for (gi <- g.indices if gi + pr <= gr) {
        for (gj <- g(gi).indices if gj + pc <= gc) {
          val isEqual =
            (0 until pr).forall(pi =>
              (0 until pc).forall(pj =>
                g(gi + pi)(gj + pj) == p(pi)(pj)
              )
            )
          if (isEqual) return true
        }
      }
      false
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val t = sc.nextInt()
      (1 to t).foreach { _ =>
        val gr, gc = sc.nextInt()
        val g = Array.fill(gr)(sc.next().toCharArray)
        val pr, pc = sc.nextInt()
        val p = Array.fill(pr)(sc.next().toCharArray)
        println(if (contains(gr, gc, g, pr, pc, p)) "YES" else "NO")
      }
    }
  }

}

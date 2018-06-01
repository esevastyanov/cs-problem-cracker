package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/stone-division-2/problem
  */
object Stone_Division_Revisited extends App
{
  Solution.main(args)

  object Solution
  {

    def stoneDivision(n: Long, ss: Array[Long]): Long = {
      val factors = ss.filter(f => f < n && n % f == 0).map(f => (f, 1L)).sorted
      for (i <- factors.length - 1 to 0 by -1) {
        val (f_i, ops_i) = factors(i)
        val piles_i = n / f_i
        for (j <- i - 1 to 0 by -1) {
          val (f_j, ops_j) = factors(j)
          if (f_i % f_j == 0 && ops_j < ops_i + piles_i) {
            factors(j) = (f_j, ops_i + piles_i)
          }
        }
      }
      if (factors.nonEmpty) factors.maxBy(_._2)._2 else 0
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val n = sc.nextLong()
        val ss = Array.fill(sc.nextInt())(sc.nextLong())
        println(stoneDivision(n, ss))
      }
    }
  }

}

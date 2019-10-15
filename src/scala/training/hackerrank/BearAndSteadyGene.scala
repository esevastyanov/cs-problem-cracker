package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/bear-and-steady-gene/problem
  */
object BearAndSteadyGene extends App
{
  Solution.main(args)

  object Solution
  {
    def sub(a: Array[Int], b: Array[Int]): Array[Int] = a.zip(b).map { case (ai, bi) => ai - bi }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val gene = sc.next()
      val s = Array.fill(n)(Array.fill(4)(0))
      var p = Array.fill(4)(0)
      for (i <- 0 until n) {
        for (j <- 0 until 4) {
          s(i)(j) = p(j)
        }
        gene(i) match {
          case 'A' => s(i)(0) += 1
          case 'C' => s(i)(1) += 1
          case 'G' => s(i)(2) += 1
          case 'T' => s(i)(3) += 1
        }
        p = s(i)
      }
      if (s.last.forall(_ == n / 4)) {
        println(0)
        return
      }
      val toRemove = s.last.map(_ - n / 4).map(math.max(0, _))
      // https://www.geeksforgeeks.org/find-the-smallest-window-in-a-string-containing-all-characters-of-another-string/
      var i = -1
      var j = 0
      var minLen = n
      while (i < n && j <= n) {
        val si = if (i >= 0) s(i) else Array.fill(4)(0)
        val sj = s(math.min(j, n - 1))
        val predArr = sub(sub(sj, si), toRemove)
        if (predArr.forall(_ >= 0)) {
          minLen = math.min(j - i, minLen)
          i += 1
        }
        if (predArr.exists(_ < 0)) {
          j += 1
        }
        if (i == j) {
          j += 1
        }
      }
      println(minLen)
    }
  }

}

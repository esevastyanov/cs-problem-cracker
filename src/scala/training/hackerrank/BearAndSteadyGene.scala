package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/bear-and-steady-gene/problem
  */
object BearAndSteadyGene extends App
{
  Solution.main(args)

  object Solution
  {
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
      for (i <- s.last.max - n / 4 to n) {
        for (j <- 0 until n - i) {
          if ((0 until 4).forall(k => n / 4 >= (if (j != 0) s(j - 1)(k) else 0) + s(n - 1)(k) - s(j + i - 1)(k))) {
            println(i)
            return
          }
        }
      }
      println(n)
    }
  }

}

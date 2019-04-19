package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/organizing-containers-of-balls/problem
  */
object Organizing_Containers_of_Balls extends App
{
  Solution.main(args)

  object Solution
  {

    def isSortable(n: Int, m: Array[Array[Int]]): Boolean = {
      val buckets = m.map(_.sum).sorted
      val balls = Array.fill(n)(0)
      for (i <- 0 until n)
        for (j <- 0 until n)
          balls(j) += m(i)(j)
      balls.sorted.zip(buckets).takeWhile { case (a, b) => a == b }.size == buckets.size
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val n = sc.nextInt()
        val m = Array.fill(n)(Array.fill(n)(sc.nextInt()))
        println(if (isSortable(n,m)) "Possible" else "Impossible")
      }
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/common-child/problem
  */
object Common_Child extends App
{
  Solution.main(args)

  object Solution
  {

    def maxChildLength(s1: String, s2: String): Int = {
      val r = Array.fill(s1.length)(Array.fill(s2.length)(0))
      r.indices.foreach { i =>
        r(i).indices.foreach { j =>
          r(i)(j) = {
            val eq = if (s1(i) == s2(j)) 1 else 0
            val i_1 = if (i > 0) r(i - 1)(j) else 0
            val j_1 = if (j > 0) r(i)(j - 1) else 0
            val i_j_1 = if (i > 0 && j > 0) r(i-1)(j-1) else 0
            math.max(math.max(i_1, j_1), i_j_1 + eq)
          }
        }
      }
      r(s1.length - 1)(s2.length - 1)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s1, s2 = sc.next()
      println(maxChildLength(s1, s2))
    }
  }

}

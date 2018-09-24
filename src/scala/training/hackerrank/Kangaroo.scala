package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/kangaroo/problem
  */
object Kangaroo extends App
{
  Solution.main(args)

  object Solution
  {

    def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
      if (x1 == x2) return "YES"
      if (v1 == v2) return "NO"
      val t = (x1 - x2) / (v2 - v1)
      if (t < 0) return "NO"
      if (x1 + v1 * t == x2 + v2 * t) "YES" else "NO"
    }


    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val x1, v1, x2, v2 = sc.nextInt()
      println(kangaroo(x1, v1, x2, v2))
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximizing-xor/problem
  */
object Maximizing_XOR extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val a = sc.nextInt()
      val b = sc.nextInt()
      val min = math.min(a, b)
      val max = math.max(a, b)
      var shift = 31
      while ((min >>> shift) == (max >>> shift)) shift -= 1
      println((1L << (shift + 1)) - 1)
    }
  }

}

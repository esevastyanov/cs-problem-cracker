package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/sum-vs-xor/problem
  */
object Sum_vs_XOR extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextLong()
      var shift = 63
      while (shift >= 0 && (n >>> shift) == 0) shift -= 1
      println(1 << (1 + shift - java.lang.Long.bitCount(n)))
    }
  }

}

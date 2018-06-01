package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/lonely-integer/problem
  */
object Lonely_Integer extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      var res = 0
      (1 to sc.nextInt()).foreach(_ => res ^= sc.nextInt())
      println(res)
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/extra-long-factorials/problem
  */
object Extra_Long_Factorials extends App
{
  Solution.main(args)

  object Solution
  {

    def factorial(n: Int): BigInt = (BigInt(1) /: (2 to n))(_ * _)

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      println(factorial(n))
    }
  }

}

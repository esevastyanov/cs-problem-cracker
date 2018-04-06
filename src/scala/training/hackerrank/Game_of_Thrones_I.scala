package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/game-of-thrones/problem
  */
object Game_of_Thrones_I extends App
{
  Solution.main(args)

  object Solution
  {

    def isPalindrome(s: String): String = {
      val m = s.groupBy(identity)
      if (m.values.count(_.length % 2 == 1) == s.length % 2) "YES" else "NO"
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      println(isPalindrome(sc.next()))
    }
  }

}

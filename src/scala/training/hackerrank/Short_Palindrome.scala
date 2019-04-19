package training.hackerrank


/**
  * https://www.hackerrank.com/challenges/short-palindrome/problem
  */
object Short_Palindrome extends App
{
  Solution.main(args)

  object Solution
  {

    def shortPalindrome(s: String): Long = {
      val as = 'z' - 'a' + 1
      val arr = Array.fill(as)(Array.fill(as)(Array.fill(4)(0L)))
      s.foreach { si =>
        val i = si - 'a'
        (0 until as).foreach { j =>
          arr(i)(j)(3) += arr(i)(j)(2) % 1000000007  // N(ijji) += N(ijj)
          arr(j)(i)(2) += arr(j)(i)(1) % 1000000007  // N(jii) += N(ji)
          arr(j)(i)(1) += arr(j)(j)(0) % 1000000007  // N(ji) += N(j)
        }
        arr(i)(i)(0) += 1                            // N(i) += 1
      }
      var total = 0L
      (0 until as).foreach(i =>
        (0 until as).foreach(j =>
          total = (total + arr(i)(j)(3)) % 1000000007
        )
      )
      total
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s = sc.next()
      println(shortPalindrome(s))
    }
  }

}

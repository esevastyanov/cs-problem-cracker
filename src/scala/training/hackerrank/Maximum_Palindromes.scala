package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximum-palindromes/problem
  */
object Maximum_Palindromes extends App
{
  Solution.main(args)

  object Solution
  {

    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n-1)

    def countMaxPalindromes(s: String, i: Int, j: Int): Int = {
      val a = Array.fill('z' - 'a' + 1)(0)
      for (k <- i to j) {
        a(s(k) - 'a') += 1
      }
      val halves = a.map(_ / 2)
      factorial(halves.sum) / halves.map(factorial).product * math.max(1, a.count(_ % 2 == 1))
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s = sc.next()
      (1 to sc.nextInt()).foreach { _ =>
        val i, j = sc.nextInt()
        println(countMaxPalindromes(s, i - 1, j - 1))
      }
    }
  }

}

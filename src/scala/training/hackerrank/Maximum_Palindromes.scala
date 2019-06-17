package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximum-palindromes/problem
  */
object Maximum_Palindromes extends App
{
  Solution.main(args)

  object Solution
  {

    val cache: Array[BigInt] = Array.fill(100001)(BigInt(0))
    cache(0) = 1
    cache(1) = 1

    def factorial(n: Int): BigInt = {
      if (cache(n) != 0) cache(n)
      val res = n * factorial(n - 1)
      cache(n) = res
      res
    }

    def countMaxPalindromes(s: String, i: Int, j: Int): Int = {
      val a = Array.fill('z' - 'a' + 1)(0)
      for (k <- i to j) {
        a(s(k) - 'a') += 1
      }
      val halves = a.map(_ / 2)
      val p = 1000000007
      val res = factorial(halves.sum) / halves.map(factorial).product * math.max(1, a.count(_ % 2 == 1)) % p
      res.toInt
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

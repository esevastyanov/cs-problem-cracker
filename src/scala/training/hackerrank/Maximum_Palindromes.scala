package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximum-palindromes/problem
  */
object Maximum_Palindromes extends App
{
  Solution.main(args)

  object Solution
  {

    val cache: Array[Long] = Array.fill(100001)(0L)
    cache(0) = 1L
    cache(1) = 1L

    def factorial(n: Int, p: Int): Long = {
      if (cache(n) != 0) return cache(n)
      val res = factorial(n - 1, p) * n % p
      cache(n) = res
      res
    }

    def exp(n: Long, e: Int, p: Int): Long = {
      if (e == 0) return 1
      if (e == 1) return n
      if (e % 2 == 0) exp(n * n % p, e / 2, p)
      else n * exp(n * n % p, (e - 1) / 2, p) % p
    }

    def countMaxPalindromes(s: String, i: Int, j: Int): Long = {
      val a = Array.fill('z' - 'a' + 1)(0)
      for (k <- i to j) {
        a(s(k) - 'a') += 1
      }
      val halves = a.map(_ / 2)
      val p = 1000000007
      val res =
        ((factorial(halves.sum, p) * (halves.map(factorial(_, p)).map(exp(_, p-2, p)).reduce(_ * _ % p) % p) % p) * math.max(1, a.count(_ % 2 == 1))) % p
      res
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

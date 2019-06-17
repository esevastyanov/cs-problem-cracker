package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximum-palindromes/problem
  */
object Maximum_Palindromes extends App
{
  Solution.main(args)

  object Solution
  {

    def factorial(n: Long): Long = if (n == 0) 1 else n * factorial(n-1)

    def countMaxPalindromes(s: String, i: Int, j: Int): Long = {
      val a = Array.fill('z' - 'a' + 1)(0)
      for (k <- i to j) {
        a(s(k) - 'a') += 1
      }
      val halves = a.map(_ / 2)
      val sum = (1 to halves.sum).map(_.toLong).grouped(10).map(_.product).toArray
      val products = halves.filter(_ > 1).map(p => (2 to p).map(_.toLong).grouped(10).map(_.product).toArray)
      products.foreach { ps =>
        for (pi <- ps.indices if ps(pi) != 1) {
          for (si <- sum.indices if sum(si) != 1) {
            if (ps(pi) % sum(si) == 0) {
              ps(pi) /= sum(si)
              sum(si) = 1
            } else if (sum(si) % ps(pi) == 0) {
              sum(si) /= ps(pi)
              ps(pi) = 1
            }
          }
        }
      }
      val num = 1000000007
      val res = sum.filter(_ > 1).map(BigInt(_)).product % num * math.max(1, a.count(_ % 2 == 1))
      res.toLong
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

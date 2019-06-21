package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/maximum-palindromes/problem
  */
object Maximum_Palindromes extends App
{
  Solution.main(args)

  object Solution
  {

    val p = 1000000007

    val f_cache: Array[Long] = {
      val a = Array.fill(100001)(1L)
      (2 until 100001).foreach(i => a(i) = i * a(i - 1) % p)
      a
    }

    val e_cache: collection.mutable.ListBuffer[Long] = {
      val l = collection.mutable.ListBuffer[Long]()
      var e = p - 2
      while (e != 1) {
        l += e % 2
        e /= 2
      }
      l.reverse
    }

    def exp(n: Long, e: Int, p: Int): Long = {
      if (e == 0) return 1
      if (e == 1) return n
      var total = n
      e_cache.foreach {
        case 0 => total = total * total % p
        case 1 => total = ((n * total) % p * total) % p
      }
      total
    }

    def countMaxPalindromes(a: Array[Int]): Long = {
      val halves = a.map(_ / 2)
      val res =
        (f_cache(halves.sum) * halves.map(f_cache(_)).map(exp(_, p-2, p)).reduce(_ * _ % p) % p) *
          math.max(1, a.count(_ % 2 == 1)) % p
      res
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s = sc.next()
      val arr = Array.fill(s.length)(Array.fill('z' - 'a' + 1)(0))
      arr(0)(s(0) - 'a') += 1
      (1 until s.length).foreach { i =>
        arr(i).indices.foreach(j => arr(i)(j) = arr(i-1)(j))
        arr(i)(s(i) - 'a') += 1
      }
      (1 to sc.nextInt()).foreach { _ =>
        val i, j = sc.nextInt() - 1
        val ai = if (i == 0) Array.fill('z' - 'a' + 1)(0) else arr(i - 1)
        val aj = arr(j)
        val a = aj.zip(ai).map { case (jj, ii) => jj - ii }
        println(countMaxPalindromes(a))
      }
    }
  }

}

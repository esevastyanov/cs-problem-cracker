package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/repeat-k-sums/problem
  */
object Repetitive_K_Sums extends App
{

  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def count(n: Int, k: Int): Int = {
      var c = 0

      def _do(d: Int, p: Int): Unit =
        if (d == 0) c += 1 else (p to n).foreach(_do(d - 1, _))

      _do(k, 1)
      c
    }

    def possibleKSums(k: Int, es: mutable.ArrayBuffer[Long]): mutable.Map[Long, Int] = {
      val sums = mutable.Map[Long, Int]().withDefaultValue(0)

      def _do(d: Int, p: Int, s: Long): Unit =
        if (d == 0) sums(s) += 1 else (p until es.size).foreach(i => _do(d - 1, i, s + es(i)))

      _do(k, 0, 0)
      sums
    }

    def elems(n: Int, k: Int, ksums: Array[Long]): mutable.ArrayBuffer[Long] = {
      var i = 0
      var si = 0L
      val es = mutable.ArrayBuffer[Long]()
      val found = mutable.Map[Long, Int]().withDefaultValue(0)
      var toFind = mutable.Map[Long, Int]().withDefaultValue(0)
      while (es.size != n) {
        while (toFind {si = ksums(i); si} > 0) {
          toFind(si) -= 1
          found(si) += 1
          i += 1
        }
        es += (if (es.isEmpty) si / k else si - es.head * (k - 1))
        found(si) += 1
        toFind = possibleKSums(k, es).map { case (s, c) => (s, c - found(s)) }.withDefaultValue(0)
        i += 1
      }
      es
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val n = sc.nextInt()
        val k = sc.nextInt()
        val ksums = Array.fill(count(n, k))(sc.nextLong())
        println(elems(n, k, ksums.sorted).mkString(" "))
      }
    }
  }

}

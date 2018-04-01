package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/common-child/problem
  */
object Common_Child extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def maxChildLength(s1: String, s2: String): Int = {
      val (sMinL, sMaxL) = if (s1.length < s2.length) (s1, s2) else (s2, s1)
      val charsMap = Array.fill('Z' - 'A' + 1)(mutable.ArrayBuffer[Int]())
      (0 until sMaxL.length).foreach { i => charsMap('Z' - sMaxL(i)) += i }

      def _count(i: Int, n: Int): Int = {
        if (i >= sMinL.length) {
          0
        } else {
          val c = sMinL(i)
          charsMap('Z' - c)
            .find(_ > n)
            .map(nn => math.max(1 + _count(i + 1, nn), _count(i + 1, n)))
            .getOrElse(_count(i + 1, n))
        }
      }

      _count(0, -1)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s1, s2 = sc.next()
      println(maxChildLength(s1, s2))
    }
  }

}

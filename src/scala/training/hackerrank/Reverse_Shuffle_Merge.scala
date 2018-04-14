package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/reverse-shuffle-merge/problem
  */
object Reverse_Shuffle_Merge extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    def findExtremePoint(s: String, m: Map[Char, Int], start: Int, end: Int): Int = {
      val mm = mutable.Map().withDefaultValue(0) ++ m
      var i = start
      while (i < end) {
        val l = s(i)
        if (mm(l) <= 1) mm -= l else mm(l) -= 1
        if (mm.isEmpty) return i
        i += 1
      }
      ???
    }

    def findMin(s: String, m: Map[Char, Int], start: Int, end: Int): Int = {
      var minC = Char.MaxValue
      var minI = -1
      (start to end).foreach { i =>
        val c = s(i)
        if (m.contains(c)) {
          if (minC == c) minI = i
          else if (minC >= c) {
            minC = c
            minI = i
          }
        }
      }
      minI
    }

    def reverseShuffleMerge(s: String): String =  {
      var letters = mutable.Map[Char, Int]().withDefaultValue(0)
      s.indices.foreach { i =>
        val l = s(i)
        letters(l) += 1
      }
      letters = letters.map {case (k, v) => (k, v/2)}
      val sb = new mutable.StringBuilder()
      var ep = s.length
      var minI = s.length - 1
      while (letters.nonEmpty) {
        ep = findExtremePoint(s, letters.toMap, 0, minI)
        minI = findMin(s, letters.toMap, ep, minI)
        val c = s(minI)
        sb += c
        if (letters(c) <= 1) letters -= c else letters(c) -= 1
      }
      sb.toString()
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      println(reverseShuffleMerge(sc.next()))
    }
  }

}

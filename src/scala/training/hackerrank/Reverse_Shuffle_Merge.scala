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
      (start to end).foreach { i =>
        val c = s(i)
        if (mm(c) <= 1) mm -= c else mm(c) -= 1
        if (mm.isEmpty) return i
      }
      end
    }

    def findMinPoint(s: String, m: Map[Char, Int], start: Int, end: Int): Int = {
      var minc = Char.MaxValue
      var mini = -1
      (start to end).foreach { i =>
        val c = s(i)
        if (m.contains(c) && minc >= c) {
          minc = c
          mini = i
        }
      }
      mini
    }

    def reverseShuffleMerge(s: String): String = {
      val doubledLetters = mutable.Map[Char, Int]().withDefaultValue(0)
      s.indices.foreach { i =>
        val l = s(i)
        doubledLetters(l) += 1
      }
      val letters = doubledLetters.map { case (k, v) => (k, v / 2) }
      val sb = new mutable.StringBuilder()
      var extremePoint = s.length
      var minPoint = s.length
      while (letters.nonEmpty) {
        extremePoint = findExtremePoint(s, letters.toMap, 0, minPoint - 1)
        minPoint = findMinPoint(s, letters.toMap, extremePoint, minPoint - 1)
        val c = s(minPoint)
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

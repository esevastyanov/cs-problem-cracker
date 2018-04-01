package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/sherlock-and-valid-string/problem
  */
object Sherlock_Valid_String extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      var s = sc.next()
      val m = mutable.Map[Char, Int]().withDefaultValue(0)
      for (i <- 0 until s.length) {
        val ch = s(i)
        m.update(ch, m(ch) + 1)
      }
      val vs = m.values
      val set = vs.toSet
      if (set.size == 1) {
        println("YES")
        return
      }
      if (set.size > 2) {
        println("NO")
        return
      }
      val min = vs.min
      val max = vs.max
      val nMin = vs.count(_ == min)
      val nMax = vs.size - nMin
      if ((nMin == 1 && min == 1) || (nMax == 1 && max - min == 1)) {
        println("YES")
      } else {
        println("NO")
      }
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/morgan-and-a-string/problem
  */
object Morgan_and_String extends App
{

  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      var t = sc.nextInt()
      (1 to t).foreach { _ =>
        val s1 = sc.next()
        val s2 = sc.next()
        var i1, i2 = 0
        val sb = new mutable.StringBuilder()
        while (i1 != s1.length || i2 != s2.length) {
          if (i1 == s1.length) {
            sb += s2(i2)
            i2 += 1
          } else if (i2 == s2.length) {
            sb += s1(i1)
            i1 += 1
          } else if (s1(i1) < s2(i2)) {
            sb += s1(i1)
            i1 += 1
          } else {
            sb += s2(i2)
            i2 += 1
          }
        }
        println(sb.toString())
      }
    }

  }

}

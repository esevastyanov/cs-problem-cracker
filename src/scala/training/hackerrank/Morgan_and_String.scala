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
          } else if (s2(i2) < s1(i1)) {
            sb += s2(i2)
            i2 += 1
          } else {
            val e = s1(i1)
            var ii1 = i1
            var ii2 = i2
            while (ii1 < s1.length && ii2 < s2.length && s1(ii1) == s2(ii2) && s1(ii1) == e) {
              sb += s1(ii1)
              ii1 += 1
              ii2 += 1
            }
            while (ii1 < s1.length && ii2 < s2.length && s1(ii1) == s2(ii2) && s1(ii1) < e) {
              sb += s1(ii1)
              ii1 += 1
              ii2 += 1
            }
            if (ii1 == s1.length) {
              i2 = ii2
            } else if (ii2 == s2.length) {
              i1 = ii1
            } else if (s1(ii1) == s2(ii2)) {
              if (s1(ii1) > e) {
                i1 = ii1
              } else {
                var iii1 = ii1
                var iii2 = ii2
                while (iii1 < s1.length && iii2 < s2.length && s1(iii1) == s2(iii2)) {
                  iii1 += 1
                  iii2 += 1
                }
                if (iii1 == s1.length) {
                  i2 = ii2
                } else if (iii2 == s2.length) {
                  i1 = ii1
                } else if (s1(iii1) < s2(iii2)) {
                  i1 = ii1
                } else {
                  i2 = ii2
                }
              }
            } else if (s1(ii1) < s2(ii2)) {
              i1 = ii1
            } else {
              i2 = ii2
            }
          }
        }
        println(sb.toString())
      }
    }

  }

}

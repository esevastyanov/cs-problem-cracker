package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/bigger-is-greater/problem
  */
object Bigger_is_Greater extends App
{
  Solution.main(args)

  object Solution
  {

    def biggerIsGreater(w: String): String = {
      if (w.length == 1) return "no answer"
      var k = w.length - 2
      while (k >= 0 && w(k) >= w(k + 1)) {
        k -= 1
      }
      if (k < 0) return "no answer"
      val wk = w(k)
      var l = k + 1
      while (l < w.length && w(l) > w(k)) {
        l += 1
      }
      l -= 1
      val sb = new StringBuilder
      (0 until k).foreach(i => sb.append(w(i)))
      sb.append(w(l))
      (w.length-1 until k by -1).foreach { i =>
        sb.append(if (i==l) wk else w(i))
      }
      sb.mkString
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val t = sc.nextInt()
      (1 to t).foreach { _ =>
        val s = sc.next()
        println(biggerIsGreater(s))
      }
    }
  }

}

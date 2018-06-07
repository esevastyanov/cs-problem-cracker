package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/aorb/problem
  */
object A_Or_B extends App
{
  Solution.main(args)

  object Solution
  {

    def aOrB(k: Int, a: String, b: String, c: String): Unit = {
      val size = List(a.length, b.length, c.length).max
      val combined =
        a.reverse
          .zipAll(b.reverse, '0', '0')
          .zipAll(c.reverse, ('0', '0'), '0')
          .reverse
          .map { case ((ai, bi), ci) =>
            (Integer.parseInt(ai.toString, 16), Integer.parseInt(bi.toString, 16), Integer.parseInt(ci.toString, 16))
          }
      var n = k
      var fixed = combined.map { case (ai, bi, ci) =>
        var _ai, _bi = 0
        for (i <- 3 to 0 by -1) {
          _ai <<= 1; _bi <<= 1
          val (ba, bb, bc) = ((ai >> i) & 1, (bi >> i) & 1, (ci >> i) & 1)
          if ((ba | bb) != bc) {
            n -= math.max(1, ba + bb)
            if (ba == 0 && bb == 0) _bi |= 1
          } else {
            _ai |= ba; _bi |= bb
          }
        }
        if (n < 0) {
          println(-1)
          return
        }
        (_ai, _bi, ci)
      }
      fixed = fixed.map { case (nai, nbi, nci) =>
        var _nai, _nbi = 0
        for (i <- 3 to 0 by -1) {
          _nai <<= 1; _nbi <<= 1
          val (ba, bb) = ((nai >> i) & 1, (nbi >> i) & 1)
          (ba, bb) match {
            case (1, 0) if n >= 2 => n -= 2; _nbi |= 1
            case (1, 1) if n >= 1 => n -= 1; _nbi |= 1
            case _ => _nai |= ba; _nbi |= bb
          }
        }
        (_nai, _nbi, nci)
      }
      val aStr = fixed.map(_._1).dropWhile(_ == 0).map(_.toHexString.toUpperCase).mkString("")
      val bStr = fixed.map(_._2).dropWhile(_ == 0).map(_.toHexString.toUpperCase).mkString("")
      println(if (aStr != "") aStr else "0")
      println(if (bStr != "") bStr else "0")
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val k = sc.nextInt()
        val a, b, c = sc.next()
        aOrB(k, a, b, c)
      }
    }
  }

}

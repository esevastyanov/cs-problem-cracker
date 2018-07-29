package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/encryption/problem
  */
object Encryption extends App
{
  Solution.main(args)

  object Solution
  {

    def encryption(s: String): String = {
      val c = math.ceil(math.sqrt(s.length)).toInt
      val r = if (c * c < s.length) c + 1 else c
      val sb = new StringBuilder
      for (j <- 0 until c) {
        for (i <- 0 until r) {
          val idx = i * c + j
          if (idx < s.length) sb += s(idx)
        }
        sb += ' '
      }
      sb.toString()
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      println(encryption(sc.next()))
    }
  }

}

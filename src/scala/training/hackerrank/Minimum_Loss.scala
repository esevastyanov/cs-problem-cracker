package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/minimum-loss/problem
  */
object Minimum_Loss extends App
{
  Solution.main(args)

  object Solution
  {

    def minimumLoss(ns: Array[Long]): Int = {
      val zipped = ns.zipWithIndex.sortBy(_._1)
      var m = Long.MaxValue
      for (i <- 0 until ns.length - 1) {
        val loss =
          if (zipped(i)._2 < zipped(i + 1)._2) {
            zipped(i + 1)._1 - zipped(i)._1
          } else {
            zipped(i)._1 - zipped(i + 1)._1
          }
        if (loss < 0) m = math.min(m, -loss)
      }
      m.toInt
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val ns = Array.fill(sc.nextInt())(sc.nextLong())
      println(minimumLoss(ns))
    }
  }

}

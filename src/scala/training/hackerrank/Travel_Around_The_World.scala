package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/travel-around-the-world/problem
  */
object Travel_Around_The_World extends App
{
  Solution.main(args)

  object Solution
  {

    def travelAroundTheWorld(n: Int, c: Long, a: Array[Long], b: Array[Long]): Int = {
      val m = Array.fill(n)(Array.fill(n)(0L))
      for (i <- 0 until n) {
        for (j <- i + 1 until n + i + 1) {
          val prev = (j + n - 1) % n
          if (m(i)(prev) >= 0) {
            m(i)(j % n) = math.min(m(i)(prev) + a(prev), c) - b(prev)
          } else {
            m(i)(j % n) = m(i)(prev)
          }
        }
      }
      (0 until n).count(i => m(i)(i) >= 0)
    }

    def _travelAroundTheWorld(n: Int, c: Long, a: Array[Long], b: Array[Long]): Int = {
      var total = 0
      for (i <- 0 until n) {
        var tank = 0L
        for (j <- i + 1 until n + i + 1) {
          val prev = (j + n - 1) % n
          if (tank >= 0) {
            tank = math.min(tank + a(prev), c) - b(prev)
          }
        }
        if (tank >= 0) total += 1
      }
      total
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val c = sc.nextLong()
      val a = Array.fill(n)(sc.nextLong())
      val b = Array.fill(n)(sc.nextLong())
      println(_travelAroundTheWorld(n, c, a, b))
    }
  }

}

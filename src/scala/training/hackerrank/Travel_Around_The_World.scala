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
      val d = Array.fill(n)(0L)
      val dmin = Array.fill(n)(0L)
      (0 until n).foreach { i => d(i) = a(i) - b(i) }
      dmin(n - 1) = d(n - 1)
      (n - 2 to 0 by -1).foreach { i => dmin(i) = math.min(d(i), d(i) + dmin(i + 1)) }
      dmin(n - 1) = math.min(d(n - 1), d(n - 1) + dmin(0))
      (n - 2 to 0 by -1).foreach { i => dmin(i) = math.min(d(i), d(i) + dmin(i + 1)) }
      (0 until n).foreach { i => if (a(i) - dmin(i) > c) return 0 }
      dmin.count(_ >= 0)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val c = sc.nextLong()
      val a = Array.fill(n)(sc.nextLong())
      val b = Array.fill(n)(sc.nextLong())
      println(travelAroundTheWorld(n, c, a, b))
    }

  }

}

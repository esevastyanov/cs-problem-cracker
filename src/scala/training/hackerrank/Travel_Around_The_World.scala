package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/travel-around-the-world/problem
  */
object Travel_Around_The_World extends App
{
  Solution.main(args)

  object Solution
  {

    def travelAroundTheWorld(n: Int, c: Long, a: Array[Int], b: Array[Int]): Int = {
      var total = n

      def check(i: Int, f: Int): Unit = {
        if (math.min(a(i), c) + f < b(i) && total > 0) {
          total -= 1
          check((i - 1 + n) % n, f + math.min(a(i), c).toInt - b(i))
        }
      }

      a.indices.foreach { i =>
        if (b(i) > c) return 0
        if (a(i) < b(i)) check(i, 0)
      }
      total
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val c = sc.nextLong()
      val a = Array.fill(n)(sc.nextInt())
      val b = Array.fill(n)(sc.nextInt())
      println(travelAroundTheWorld(n, c, a, b))
    }
  }

}

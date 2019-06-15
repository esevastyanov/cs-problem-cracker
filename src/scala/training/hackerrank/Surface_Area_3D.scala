package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/3d-surface-area/problem
  */
object Surface_Area_3D extends App
{
  Solution.main(args)

  object Solution
  {

    def surfaceArea(a: Array[Array[Int]]): Int = {
      var total = 0

      def commonSurface(h: Int, i: Int, j: Int): Int =
        if (i < 0 || j < 0) 0 else 2 * math.min(h, a(i)(j))

      for (i <- a.indices) {
        for (j <- a(i).indices) {
          total += 4 * a(i)(j) + 2
          total -= commonSurface(a(i)(j), i - 1, j)
          total -= commonSurface(a(i)(j), i, j - 1)
        }
      }
      total
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val h, w = sc.nextInt()
      val a = Array.fill(h)(Array.fill(w)(sc.nextInt()))
      println(surfaceArea(a))
    }
  }

}

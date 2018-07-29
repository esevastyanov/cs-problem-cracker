package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/connected-cell-in-a-grid/problem
  */
object Connected_Cells_In_Grid extends App
{
  Solution.main(args)

  object Solution
  {

    def connectedCell(arr: Array[Array[Int]]): Int = {
      def _count(i: Int, j: Int): Int = {
        if (i < 0 || i >= arr.length || j < 0 || j >= arr(i).length || arr(i)(j) != 1) {
          0
        } else {
          arr(i)(j) = 2
          1 +
            _count(i - 1, j - 1) +
            _count(i - 1, j) +
            _count(i - 1, j + 1) +
            _count(i, j - 1) +
            _count(i, j + 1) +
            _count(i + 1, j - 1) +
            _count(i + 1, j) +
            _count(i + 1, j + 1)
        }
      }

      var max = 0
      for (i <- arr.indices) {
        for (j <- arr(i).indices) {
          max = math.max(max, _count(i, j))
        }
      }
      max
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, m = sc.nextInt()
      val arr = Array.fill(n)(Array.fill(m)(sc.nextInt()))
      println(connectedCell(arr))
    }
  }

}

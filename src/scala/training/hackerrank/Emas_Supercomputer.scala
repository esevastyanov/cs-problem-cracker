package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/two-pluses/problem
  */
object Emas_Supercomputer extends App
{
  Solution.main(args)

  object Solution
  {

    def makeCrossesFrom(i: Int, j: Int, grid: Array[String]): Seq[Set[(Int, Int)]] = {
      val n = grid.length
      val m = grid(i).length

      def _do(size: Int, prev: Set[(Int, Int)]): Seq[Set[(Int, Int)]] = {
        val elems = Set((i - size, j), (i + size, j), (i, j - size), (i, j + size))
        if (elems.forall { case (a, b) => a >= 0 && a < n && b >= 0 && b < m && grid(a)(b) == 'G' }) {
          val cross = prev ++ elems
          cross +: _do(size + 1, cross)
        } else {
          Nil
        }
      }

      if (grid(i)(j) == 'B') {
        Nil
      } else {
        _do(0, Set.empty)
      }
    }


    def twoPluses(grid: Array[String]): Int = {
      val crosses = grid.indices.flatMap { i =>
        grid(i).indices.flatMap { j =>
          makeCrossesFrom(i, j, grid)
        }
      }
      var maxProduct = 0
      crosses.indices.foreach {i =>
        crosses.indices.foreach { j =>
          val iCross = crosses(i)
          val jCross = crosses(j)
          if (i != j && iCross.diff(jCross).size == iCross.size) {
            maxProduct = math.max(maxProduct, iCross.size * jCross.size)
          }
        }
      }
      maxProduct
    }

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val m = sc.nextInt()
      val grid = Array.fill(n)(sc.next())
      val result = twoPluses(grid)
      println(result)
    }
  }

}

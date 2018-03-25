package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/bomber-man/problem
  */
object Bomberman_Game extends App
{
  Solution.main(args)

  object Solution
  {

    def fullGrid(grid: Array[String]): Array[String] = {
      grid.map(_.map(_ => 'O').mkString)
    }

    def inversedGrid(grid: Array[String]): Array[String] = {
      grid.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (c, j) =>
          val isMinedUp = i > 0 && grid(i - 1)(j) == 'O'
          val isMinedDown = i < grid.length - 1 && grid(i + 1)(j) == 'O'
          val isMinedLeft = j > 0 && grid(i)(j - 1) == 'O'
          val isMinedRight = j < row.length - 1 && grid(i)(j + 1) == 'O'
          if (c == 'O' || isMinedUp || isMinedDown || isMinedLeft || isMinedRight) {
            '.'
          } else {
            'O'
          }
        }.mkString
      }
    }

    def bomberMan(n: Int, grid: Array[String]): Array[String] = {
      if (n == 0) return grid
      if (n == 1) return grid
      val nn = n % 4
      nn match {
        case 0 | 2 => fullGrid(grid)
        case 1 => inversedGrid(inversedGrid(grid))
        case 3 => inversedGrid(grid)
      }
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val r = sc.nextInt()
      val c = sc.nextInt()
      val n = sc.nextInt()
      val grid = Array.fill(r)(sc.next())
      val result = bomberMan(n, grid)
      println(result.mkString("\n"))
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/count-luck/problem
  */
object Count_Luck extends App
{
  Solution.main(args)

  object Solution
  {

    def findPoint(n: Int, m: Int, map: Array[Array[Char]], p: Char): (Int, Int) = {
      (0 until n).foreach(i => (0 until m).foreach(j => if (map(i)(j) == p) return (i, j)))
      throw sys.error(s"There is no point $p")
    }

    def ways(i: Int, j: Int, n: Int, m: Int, map: Array[Array[Char]]): List[(Int, Int)] = {
      List((-1, 0), (0, -1), (1, 0), (0, 1))
        .filter { case (di, dj) => i + di >= 0 && i + di < n && j + dj >= 0 && j + dj < m }
        .filter { case (di, dj) => map(i + di)(j + dj) == '.' || map(i + di)(j + dj) == '*' }
    }

    def countUncertainties(n: Int, m: Int, map: Array[Array[Char]]): Int = {
      val (si, sj) = findPoint(n, m, map, 'M')
      val (fi, fj) = findPoint(n, m, map, '*')
      var result = 0
      def _do(i: Int, j: Int, k: Int): Unit = {
        if (i == fi && j == fj) {
          result = k
          return
        }
        map(i)(j) = '+'
        val ws = ways(i, j, n, m, map)
        ws.foreach { case (di, dj) => _do(i + di, j + dj, if (ws.size > 1) k + 1 else k) }
      }
      _do(si, sj, 0)
      result
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val n, m = sc.nextInt()
        val map = Array.fill(n)(sc.next().toCharArray)
        val k = sc.nextInt()
        println(if (countUncertainties(n, m, map) == k) "Impressed" else "Oops!")
      }
    }
  }

}

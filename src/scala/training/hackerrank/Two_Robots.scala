package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/two-robots/problem
  */
object Two_Robots extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    def minTotalDistance(n: Int, qs: Array[(Int, Int)]): Int = {
      var m1, m2 = mutable.Map[(Int, Int), State]()
      val (qa, qb) = qs(0)
      m1.put((0, qb), State(qb, 0, math.abs(qb - qa)))
      (1 until n).foreach(i => {
        val (qa, qb) = qs(i)
        m1.foreach { case (p, s) =>
          (0 to 1).foreach { j =>
            val _s = s.execute(j)(qs(i))
            if (!m2.contains(_s.p)) m2.put(_s.p, _s) else if (m2(_s.p).d > _s.d) m2(_s.p) = _s
          }
        }
        m1 = m2
        m2 = mutable.Map()
      }
      )
      m1.values.minBy(_.d).d
    }

    case class State(p1: Int = 0, p2: Int = 0, d: Int = 0)
    {
      val p: (Int, Int) = (p1, p2)
      val arr = Array(p1, p2)

      def execute(i: Int)(q: (Int, Int)): State = {
        val (qa, qb) = q
        val (_p1, _p2) = (math.min(qb, arr((i + 1) % 2)), math.max(qb, arr((i + 1) % 2)))
        val _d = (if (arr(i) == 0) 0 else math.abs(arr(i) - qa)) + math.abs(qa - qb) + d
        State(p1 = _p1, p2 = _p2, d = _d)
      }

      def execute1(q: (Int, Int)): State = execute(0)(q)

      def execute2(q: (Int, Int)): State = execute(1)(q)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val m, n = sc.nextInt()
        val qs = Array.fill(n)((sc.nextInt(), sc.nextInt()))
        println(minTotalDistance(n, qs))
      }
    }
  }
}

// 1 10 8 5 10 10 2 9 10 3 6 6 9 2 9 2 8 6 5
// 50
// 1 5 4 1 5 3 2 4 1 2 4
// 11
// 1 9 8 4 8 3 9 8 2 5 9 6 9 1 4 5 3 5 6
// 40
// 1 9 6  4 8  3 9  8 2  5 9  6 9  1 4
// 34

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/two-robots/problem
  */
object Two_Robots extends App
{
  Solution.main(args)

  object Solution
  {

    def minTotalDistance(n: Int, qs: Array[(Int, Int)]): Int = {
      val state: Array[Array[States]] = Array.fill(n)(Array.fill(n)(null))
      for (i <- 0 until n) {
        for (j <- i until n) {
          val (qa, qb) = qs(j)
          if (i == 0 && j == 0) {
            state(i)(j) = States(List(State(qb, 0, math.abs(qb - qa))))
          } else if (i == 0) {
            state(i)(j) = state(i)(j-1).execute1(qs(j))
          } else if (i == j) {
            state(i)(j) = state(i-1)(j-1).execute2(qs(j))
          } else {
//            state(i)(j) = state(i)(j-1).execute1(qs(j)).min(state(i-1)(j-1).execute2(qs(j)))
            state(i)(j) = state(i)(j-1).execute1(qs(j)).and(state(i-1)(j-1).execute2(qs(j)))
          }
        }
      }
//      (0 until n).map(state(_)(n-1).ss.head).minBy(_.d).d
      (0 until n).map(state(_)(n-1).ss.minBy(_.d).d).min
    }

    def minTotalDistanceRecursive(n: Int, qs: Array[(Int, Int)]): Int = {
      var min = Int.MaxValue
      def _do(i: Int, state: State, rr: String): Unit = {
        if (i == n) {
          min = math.min(min, state.d)
        } else {
          _do(i + 1, state.execute1(qs(i)), rr + "1")
          _do(i + 1, state.execute2(qs(i)), rr + "2")
        }
      }
      _do(0, State(), "")
      min
    }

    case class States(ss: List[State]) {
      import scala.collection.mutable

      private def normalize(arr: List[State]): List[State] = {
        val m = mutable.Map[(Int, Int), State]()
        arr.foreach { s =>
          val p = (math.min(s.p1, s.p2), math.max(s.p1, s.p2))
          if (!m.contains(p)) {
            m.put(p, s)
          } else {
            if (m(p).d < s.d) m(p) = s
          }
        }
        m.values.toList
      }

      def execute1(q: (Int, Int)): States = States(normalize(ss.map(_.execute1(q))))

      def execute2(q: (Int, Int)): States = States(normalize(ss.map(_.execute2(q))))

      def min(that: States): States = {
        val d = math.min(this.ss.minBy(_.d).d, that.ss.minBy(_.d).d)
        States(normalize(this.ss ++ that.ss).filter(_.d == d))
      }

      def and(that: States): States = {
        States(normalize(this.ss ++ that.ss))
      }
    }

    case class State(p1: Int = 0, p2: Int = 0, d: Int = 0)
    {
      def execute1(q: (Int, Int)): State = {
        val (qa, qb) = q
        this.copy(p1 = qb, d = (if (p1 == 0) 0 else math.abs(p1 - qa)) + math.abs(qa - qb) + d)
      }

      def execute2(q: (Int, Int)): State = {
        val (qa, qb) = q
        this.copy(p2 = qb, d = (if (p2 == 0) 0 else math.abs(p2 - qa)) + math.abs(qa - qb) + d)
      }

      def min(s: State): State = if (this.d <= s.d) this else s
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val m, n = sc.nextInt()
        val qs = Array.fill(n)((sc.nextInt(), sc.nextInt()))
//        println(minTotalDistanceRecursive(n, qs))
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

/*
8 0 4
8 9 10
2 9 16
2 9 24
2 9 30
4 9 34
*/

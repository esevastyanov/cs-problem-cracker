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
      val (qa, qb) = qs.head
      val minDs: Array[State] = Array.fill(n)(null)
      minDs(0) = State(
        StatePartial(qb, 0, math.abs(qa - qb)),
        StatePartial(qb, 0, math.abs(qa - qb)),
        StatePartial(0, qb, math.abs(qa - qb))
      )
      (1 until n).foreach { i =>
        val (qa, qb) = qs(i)
        val stateM1 = minDs(i - 1).min.execute1(qs(i))
        val stateM2 = minDs(i - 1).min.execute2(qs(i))
        val state11 = minDs(i - 1).s1.execute1(qs(i))
        val state12 = minDs(i - 1).s1.execute2(qs(i))
        val state21 = minDs(i - 1).s2.execute1(qs(i))
        val state22 = minDs(i - 1).s2.execute2(qs(i))
        val minState = stateM1.min(stateM2).min(state11).min(state12).min(state21).min(state22)
        minDs(i) = State(minState, state11, state22)
      }
      minDs.last.min.d
    }

    case class State(
      min: StatePartial = StatePartial(),
      s1: StatePartial = StatePartial(),
      s2: StatePartial = StatePartial()
    )

    case class StatePartial(p1: Int = 0, p2: Int = 0, d: Int = 0)
    {
      def execute1(q: (Int, Int)): StatePartial = {
        val (qa, qb) = q
        this.copy(p1 = qb, d = (if (p1 == 0) 0 else math.abs(p1 - qa)) + math.abs(qa - qb) + d)
      }

      def execute2(q: (Int, Int)): StatePartial = {
        val (qa, qb) = q
        this.copy(p2 = qb, d = (if (p2 == 0) 0 else math.abs(p2 - qa)) + math.abs(qa - qb) + d)
      }

      def min(s: StatePartial): StatePartial = if (this.d <= s.d) this else s
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

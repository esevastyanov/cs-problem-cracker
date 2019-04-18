package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem
  */
object Fraudulent_Activity_Notifications extends App
{
  Solution.main(args)

  object Solution
  {
    import java.util.PriorityQueue

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, d = sc.nextInt()
      val exs = Array.fill(n)(sc.nextInt())
      println(activityNotifications(exs, d))
    }

    def activityNotifications(exs: Array[Int], d: Int): Int = {
      val mh = new MedianHeap()
      var total = 0
      for (i <- 0 until d) mh.add(exs(i))
      for (i <- d until exs.length) {
        if (exs(i) >= mh.doubleMedian) total += 1
        mh.add(exs(i))
        mh.remove(exs(i - d))
      }
      total
    }

    class MedianHeap {
      private val ls = new PriorityQueue[Int](Ordering.Int.reverse)
      private val rs = new PriorityQueue[Int](Ordering.Int)

      def add(e: Int): Unit = {
        (if (ls.isEmpty || e <= ls.peek()) ls else rs).add(e)
        rebalance()
      }

      def remove(e: Int): Unit = {
        (if (e <= ls.peek()) ls else rs).remove(e)
        rebalance()
      }

      def doubleMedian: Int = {
        if (ls.size() == rs.size()) Option(ls.peek()).getOrElse(0) + Option(rs.peek()).getOrElse(0)
        else ls.peek() << 1
      }

      private def rebalance(): Unit = {
        while (!(ls.size() >= rs.size() && ls.size() - rs.size() <= 1)) {
          if (ls.size() > rs.size()) rs.add(ls.poll()) else ls.add(rs.poll())
        }
      }
    }
  }

}

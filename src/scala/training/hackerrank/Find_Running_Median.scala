package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/ctci-find-the-running-median/problem
  */
object Find_Running_Median extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val left = new mutable.PriorityQueue[Int]()
      val right = new mutable.PriorityQueue[Int]()(
        new scala.math.Ordering[scala.Int]
        {
          override def compare(x: Int, y: Int): Int = if (x < y) 1 else if (x == y) 0 else -1
        }
      )
      val n = sc.nextInt()
      for (i <- 0 until n) {
        val e = sc.nextInt()
        if (left.headOption.forall(_ > e)) left += e else right += e
        if (right.size > left.size) left += right.dequeue()
        else if (left.size - 1 > right.size) right += left.dequeue()
        val m =
          if (left.size == right.size) {
            (left.head + right.head) / 2.0
          } else {
            left.head
          }
        println("%.1f".format(m))
      }
    }
  }

}

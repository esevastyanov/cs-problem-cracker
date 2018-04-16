package training.hackerrank


/**
  * https://www.hackerrank.com/challenges/fighting-pits/problem
  */
object Fighting_Pits extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def whoIsWinner(t1: mutable.PriorityQueue[Int], t2: mutable.PriorityQueue[Int]): Boolean = {
      val tt1 = mutable.PriorityQueue(t1.toArray: _*)
      val tt2 = mutable.PriorityQueue(t2.toArray: _*)
      var oq = tt1
      var dq = tt2
      while (tt1.nonEmpty && tt2.nonEmpty) {
        var s = oq.head
        while (s > 0 && dq.nonEmpty) {
          s -= 1
          dq.dequeue()
        }
        val t = oq
        oq = dq
        dq = t
      }
      tt1.nonEmpty
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k, q = sc.nextInt()
      val teams = Array.fill(k + 1)(mutable.PriorityQueue[Int]())
      (1 to n).foreach { _ =>
        val s = sc.nextInt()
        val i = sc.nextInt()
        teams(i).enqueue(s)
      }
      (1 to q).foreach { _ =>
        sc.nextInt() match {
          case 1 =>
            val s = sc.nextInt()
            val i = sc.nextInt()
            teams(i).enqueue(s)
          case 2 =>
            val offender = sc.nextInt()
            val defender = sc.nextInt()
            println(if (whoIsWinner(teams(offender), teams(defender))) offender else defender)
          case c => sys.error(s"Unknown command $c")
        }
      }
    }
  }

}

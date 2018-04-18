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
    import scala.collection.mutable.ListBuffer


    private class Troop(val s: Int, var n: Int)

    private class Team
    {
      val troops: ListBuffer[Troop] = ListBuffer()

      var total: Long = 0

      private var isSorted: Boolean = false

      private val m: mutable.Map[Int, Troop] = mutable.Map()

      def sort(): Unit = {
        if (!isSorted) {
          isSorted = true
          val arr = m.keys.toArray
          java.util.Arrays.sort(arr)
          (arr.length - 1 to 0 by -1).foreach { s =>
            troops += m(arr(s))
          }
        }
      }

      def addFighter(s: Int): Unit = {
        total += s
        m.getOrElseUpdate(s, new Troop(s, 0)).n += 1
      }

      def prependFighter(s: Int): Unit = {
        total += s
        if (!isSorted) sort()
        if (troops.isEmpty || troops.head.s < s) {
          new Troop(s, 1) +=: troops
        } else {
          troops.head.n += 1
        }
      }
    }

    private class TeamBattleHolder(val team: Team)
    {
      team.sort()
      private val i  = team.troops.iterator
      private var t  = if (i.hasNext) i.next() else new Troop(0, 0)
      private var ni = t.n

      var si   : Int  = t.s
      var total: Long = team.total

      def hit(damage: Int): Unit = {
        var d = damage
        while (d > 0 && total > 0) {
          val di = math.min(ni, d)
          total -= di * si
          ni -= di
          d -= di
          if (ni == 0 && i.hasNext) {
            t = i.next()
            si = t.s
            ni = t.n
          }
        }
      }
    }

    private def whoIsWinner(t1: Team, t2: Team): Team = {
      if (t2.total == 0) return t1
      if (t1.total == 0) return t2
      if (t1.total >= t2.total) return t1
      var oq = new TeamBattleHolder(t1)
      var dq = new TeamBattleHolder(t2)
      while (oq.total < dq.total && dq.total > 0) {
        dq.hit(oq.si)
        val t = oq
        oq = dq
        dq = t
      }
      oq.team
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k, q = sc.nextInt()
      val teams = Array.fill(k + 1)(new Team())
      (1 to n).foreach { _ =>
        val s = sc.nextInt()
        val i = sc.nextInt()
        teams(i).addFighter(s)
      }
      (1 to q).foreach { _ =>
        sc.nextInt() match {
          case 1 =>
            val s = sc.nextInt()
            val i = sc.nextInt()
            teams(i).prependFighter(s)
          case 2 =>
            val offender = sc.nextInt()
            val tOff = teams(offender)
            val defender = sc.nextInt()
            val tDef = teams(defender)
            println(if (whoIsWinner(tOff, tDef) == tOff) offender else defender)
          case c => sys.error(s"Unknown command $c")
        }
      }
    }
  }

}

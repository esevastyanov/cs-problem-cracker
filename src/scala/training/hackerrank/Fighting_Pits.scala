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

    private class Team
    {
      val m: mutable.SortedMap[Int, Int] = mutable.SortedMap()

      var strength: Int = 0
      var count   : Int = 0

      private var changed = true

      private var fighters : Array[Int] = Array()
      private var strengths: Array[Int] = Array()

      def addFighter(s: Int): Unit = {
        changed = true
        val v = m.getOrElseUpdate(s, 0)
        m(s) = v + 1
        strength += s
        count += 1
      }

      def getFighters: Array[Int] = {
        refresh()
        fighters
      }

      def getStrenghts: Array[Int] = {
        refresh()
        strengths
      }

      private def refresh(): Unit = {
        if (changed) {
          changed = false
          fighters = new Array[Int](count)
          strengths = new Array[Int](count)
          var i = 0
          m.foreach { case (k, v) =>
            (1 to v).foreach { _ =>
              fighters(i) = k
              strengths(i) = if (i == 0) k else strengths(i - 1) + k
              i += 1
            }
          }
        }
      }
    }

    private class TeamBattleHolder(val team: Team)
    {
      val fighters : Array[Int] = team.getFighters
      var i        : Int        = team.getFighters.length - 1

      def total: Int = if (i >= 0) team.getStrenghts(i) else 0

      def hit(damage: Int): Unit = {
        i -= damage
      }
    }

    private def whoIsWinner(t1: Team, t2: Team): Team = {
      val tt1 = new TeamBattleHolder(t1)
      val tt2 = new TeamBattleHolder(t2)
      if (tt2.i < 0) return t1
      if (tt1.i < 0) return t2
      var oq = tt1
      var dq = tt2
      while (oq.total < dq.total && oq.i >= 0 && dq.i >= 0) {
        dq.hit(oq.fighters(oq.i))
        if (dq.i >= 0) {
          val t = oq
          oq = dq
          dq = t
        }
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
            teams(i).addFighter(s)
          case 2 =>
            val offender = sc.nextInt()
            val defender = sc.nextInt()
            val tOff = teams(offender)
            val tDef = teams(defender)
            println(if (whoIsWinner(tOff, tDef) == tOff) offender else defender)
          case c => sys.error(s"Unknown command $c")
        }
      }
    }
  }

}

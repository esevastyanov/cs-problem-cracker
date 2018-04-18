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

    private case class Team(
      t: mutable.SortedMap[Int, Int] = mutable.SortedMap()(Ordering.Int.reverse),
      var strength: Long = 0
    )

    private class TeamBattleHolder(val team: Team)
    {
      private val iterator = team.t.keys.iterator
      var strength: Int = if (iterator.hasNext) iterator.next() else 0
      private var ni = if (strength != 0) team.t(strength) else 0
      var totalStrength: Long = team.strength

      def hit(damage: Int): Unit = {
        if (damage > 0 && totalStrength > 0) {
          val di = math.min(ni, damage)
          totalStrength -= di * strength
          ni -= di
          if (ni == 0 && iterator.hasNext) {
            strength = iterator.next()
            ni = team.t(strength)
          }
          hit(damage - di)
        }
      }
    }

    private def whoIsWinner(t1: Team, t2: Team): Team = {
      var oq = new TeamBattleHolder(t1)
      var dq = new TeamBattleHolder(t2)
      if (dq.totalStrength == 0) return oq.team
      if (oq.totalStrength == 0) return dq.team
      while (oq.totalStrength < dq.totalStrength && dq.totalStrength > 0) {
        dq.hit(oq.strength)
        val t = oq
        oq = dq
        dq = t
      }
      oq.team
    }

    private def addFighter(team: Team, s: Int): Unit = {
      team.t(s) = team.t.getOrElse(s, 0) + 1
      team.strength += s
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k, q = sc.nextInt()
      val teams = Array.fill(k + 1)(Team())
      (1 to n).foreach { _ =>
        val s = sc.nextInt()
        val i = sc.nextInt()
        addFighter(teams(i), s)
      }
      (1 to q).foreach { _ =>
        sc.nextInt() match {
          case 1 =>
            val s = sc.nextInt()
            val i = sc.nextInt()
            addFighter(teams(i), s)
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

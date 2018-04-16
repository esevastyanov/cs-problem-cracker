package training.hackerrank


/**
  * https://www.hackerrank.com/challenges/fighting-pits/problem
  */
object Fighting_Pits extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.SortedMap

    class Team(val t: SortedMap[Int, Int])
    {
      private val iterator = t.keys.iterator
      var strength: Int = iterator.next()
      var health  : Int = t(strength)

      def hit(damage: Int): Unit = {
        if (health <= 0) {
          strength = if (iterator.hasNext) iterator.next() else 0
          health = t(strength)
        }
        if (damage > 0 && health > 0) {
          val h = health
          health -= damage
          hit(damage - h)
        }
      }
    }

    def whoIsWinner(t1: SortedMap[Int, Int], t2: SortedMap[Int, Int]): Boolean = {
      val tt1 = new Team(t1)
      val tt2 = new Team(t2)
      var oq = tt1
      var dq = tt2
      while (oq.health > 0 && dq.health > 0) {
        dq.hit(oq.strength)
        val t = oq
        oq = dq
        dq = t
      }
      tt1.health > 0
    }

    private def addFighter(team: SortedMap[Int, Int], s: Int) = {
      team.updated(s, team.getOrElse(s, 0) + 1)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k, q = sc.nextInt()
      val teams = Array.fill(k + 1)(SortedMap[Int, Int](0 -> 0)(Ordering.Int.reverse))
      (1 to n).foreach { _ =>
        val s = sc.nextInt()
        val i = sc.nextInt()
        teams(i) = addFighter(teams(i), s)
      }
      (1 to q).foreach { _ =>
        sc.nextInt() match {
          case 1 =>
            val s = sc.nextInt()
            val i = sc.nextInt()
            teams(i) = addFighter(teams(i), s)
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

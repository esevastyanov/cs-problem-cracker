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


    private class Troop(val s: Int, var n: Long)

    private class Team(val i: Int)
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
          d -= di.toInt
          if (ni == 0 && i.hasNext) {
            t = i.next()
            si = t.s
            ni = t.n
          }
        }
      }
    }

    var cache: mutable.Map[(Int, Int), Int] = mutable.Map[(Int, Int), Int]()

    private def whoIsWinner(t1: Team, t2: Team): Int = {
      if (t2.total == 0) return t1.i
      if (t1.total == 0) return t2.i
      if (t1.total >= t2.total) return t1.i
      cache.getOrElseUpdate((t1.i, t2.i), {
        var oq = new TeamBattleHolder(t1)
        var dq = new TeamBattleHolder(t2)
        while (oq.total < dq.total && dq.total > 0) {
          dq.hit(oq.si)
          val t = oq
          oq = dq
          dq = t
        }
        oq.team.i
      })
    }

    def main(args: Array[String]): Unit = {
      val sc = new FastReader
      val n, k, q = sc.nextInt()
      val teams = new Array[Team](k + 1)
      (1 to n).foreach { i =>
        val s = sc.nextInt()
        val i = sc.nextInt()
        if (teams(i) == null) teams(i) = new Team(i)
        teams(i).addFighter(s)
      }
      (1 to q).foreach { i =>
        sc.nextInt() match {
          case 1 =>
            cache = cache.empty
            val s = sc.nextInt()
            val i = sc.nextInt()
            if (teams(i) == null) teams(i) = new Team(i)
            teams(i).prependFighter(s)
          case 2 =>
            val offender = sc.nextInt()
            val defender = sc.nextInt()
            val tOff = Option(teams(offender)).getOrElse(new Team(offender))
            val tDef = Option(teams(defender)).getOrElse(new Team(defender))
            val winner = whoIsWinner(tOff, tDef)
            println(winner)
          case c => sys.error(s"Unknown command $c")
        }
      }
    }

    class FastReader()
    {

      import java.io.DataInputStream

      private val BUFFER_SIZE   = 1 << 16
      private val din           = new DataInputStream(System.in)
      private val buffer        = new Array[Byte](BUFFER_SIZE)
      private var bufferPointer = 0
      private var bytesRead     = 0


      def nextInt(): Int = {
        var ret = 0
        var c = read
        while (c <= ' ') {
          c = read
        }
        val neg = c == '-'
        if (neg) c = read
        do {
          ret = ret * 10 + c - '0'
          c = read
        } while (c >= '0' && c <= '9')
        val res = if (neg) -ret else ret
        res
      }

      private def fillBuffer(): Unit = {
        bufferPointer = 0
        bytesRead = din.read(buffer, bufferPointer, BUFFER_SIZE)
        if (bytesRead == -1) buffer(0) = -1
      }

      private def read = {
        if (bufferPointer == bytesRead) fillBuffer()
        bufferPointer += 1
        buffer(bufferPointer - 1)
      }

      def close(): Unit = {
        if (din == null) return
        din.close()
      }
    }

  }

}

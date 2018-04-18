package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/fighting-pits/problem
  */
object Fighting_Pits extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable.ListBuffer

    private class Troop(val s: Int, var n: Int)

    private class Team
    {
      val troops: ListBuffer[Troop] = ListBuffer()

      var total: Long = 0

      private var isSorted: Boolean = false

      private val temp: ListBuffer[Int] = ListBuffer()

      def sort(): Unit = {
        if (!isSorted) {
          isSorted = true
          val arr = temp.toArray
          java.util.Arrays.sort(arr)
          arr.indices.foreach(i => _prepend(arr(i)))
        }
      }

      def addFighter(s: Int): Unit = {
        total += s
        s +=: temp
      }

      def prependFighter(s: Int): Unit = {
        total += s
        if (!isSorted) sort()
        _prepend(s)
      }

      private def _prepend(s: Int) = {
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
      val sc = new FastReader
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

      def next(): String = {
        val sb = new StringBuilder()
        var c = read
        while (c != -1 && (c == '\n' || c == ' ' || c == '\t')) {
          c = read
        }
        while (c != -1 && c != '\n' && c != ' ' && c != '\t') {
          sb.append(c.toChar)
          c = read
        }
        val res = sb.mkString
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

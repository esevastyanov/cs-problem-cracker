package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/jack-goes-to-rapture/problem
  */
object Jack_Goes_to_Rapture_2 extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    type Graph = Array[mutable.ArrayBuffer[(Int, Int)]]

    def minMaxWeight(start: Int, end: Int, g: Graph): Int = {
      implicit val _ = new Ordering[Distance] {
        override def compare(x: Distance,y: Distance): Int = {
          if (x.c == y.c) y.v - x.v else y.c - x.c
        }
      }
      val queue = mutable.PriorityQueue[Distance]()
      val dMap = mutable.Map[Int, Distance]()
      val dist = mutable.Map[Int, Distance](start -> new Distance(start, 0))
      g.indices.foreach { v =>
        val vD = dist.getOrElseUpdate(v, new Distance(v, Int.MaxValue))
        queue.enqueue(vD)
      }
      while (queue.nonEmpty) {
        val cD = {
          var d = queue.dequeue()
          while (!d.isActual && queue.nonEmpty) d = queue.dequeue()
          if (!d.isActual) return dist(end).c
          d
        }
        val cv = cD.v
        val cc = cD.c
        if (cv == end) return cc
        g(cv).foreach { case (v, c) =>
          val alt = math.max(cc, c)
          val vD = dist(v)
          if (alt < vD.c) {
            vD.isActual = false
            val newVD = new Distance(v, alt)
            dist(v) = newVD
            queue.enqueue(newVD)
          }
        }
      }
      dist(end).c
    }

    def main(args: Array[String]): Unit = {
      val sc = new Reader
      val n = sc.nextInt()
      val e = sc.nextInt()
      val g: Graph = Array.fill(n)(mutable.ArrayBuffer[(Int, Int)]())
      (1 to e).foreach { _ =>
        val (i, j, w) = (sc.nextInt(), sc.nextInt(), sc.nextInt())
        g(i-1).append(j-1 -> w)
        g(j-1).append(i-1 -> w)
      }
      val res = minMaxWeight(0, n - 1, g)
      if (res == Int.MaxValue) {
        println("NO PATH EXISTS")
      } else {
        println(res)
      }
    }

    class Distance(val v: Int, val c: Int, var isActual: Boolean = true)

    class Reader()
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
        if (neg) -ret else ret
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

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/jack-goes-to-rapture/problem
  */
object Jack_Goes_to_Rapture extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    type Graph = Array[mutable.ArrayBuffer[(Int, Int)]]

    def minMaxWeight(start: Int, end: Int, g: Graph): Int = {
      implicit val _ = new Ordering[VWeight]
      {
        override def compare(a: VWeight, y: VWeight): Int = {
          if (a.w == y.w) y.v - a.v else y.w - a.w
        }
      }
      val weightsQueue = mutable.PriorityQueue[VWeight]()
      val weightsMap = mutable.Map[Int, VWeight](start -> new VWeight(start, 0))
      g.indices.foreach { v =>
        val vw = weightsMap.getOrElseUpdate(v, new VWeight(v, Int.MaxValue))
        weightsQueue.enqueue(vw)
      }

      while (weightsQueue.nonEmpty) {
        val cvw = {
          var w = weightsQueue.dequeue()
          while (w.isStale && weightsQueue.nonEmpty) {
            w = weightsQueue.dequeue()
          }
          if (w.isStale) return weightsMap(end).w
          w
        }
        val cv = cvw.v
        val cw = cvw.w
        if (cv == end) return cw
        g(cv).foreach { case (v, w) =>
          val alt = math.max(cw, w)
          val vw = weightsMap(v)
          if (alt < vw.w) {
            vw.isStale = true
            val _vw = new VWeight(v, alt)
            weightsMap(v) = _vw
            weightsQueue.enqueue(_vw)
          }
        }
      }

      weightsMap(end).w
    }

    def main(args: Array[String]): Unit = {
      val sc = new FastReader
      val n = sc.nextInt()
      val e = sc.nextInt()
      val g: Graph = Array.fill(n)(mutable.ArrayBuffer[(Int, Int)]())
      (1 to e).foreach { _ =>
        val (i, j, w) = (sc.nextInt(), sc.nextInt(), sc.nextInt())
        g(i - 1).append(j - 1 -> w)
        g(j - 1).append(i - 1 -> w)
      }
      val res = minMaxWeight(0, n - 1, g)
      if (res == Int.MaxValue) {
        println("NO PATH EXISTS")
      } else {
        println(res)
      }
    }

    class VWeight(val v: Int, val w: Int, var isStale: Boolean = false)

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

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

    type Graph = mutable.Map[Int, mutable.Map[Int, Int]]

    private def removeUnusedLeaves(g: Graph, used: Set[Int]): Unit = {
      val removed = mutable.ArrayBuffer[Int]()

      def _do(toCheck: scala.collection.Set[Int]): Unit = {
        val nextToCheck = mutable.Set[Int]()
        (toCheck -- used).foreach { i =>
          val ns = g(i).keys
          val isLeaf = ns.size == 1
          if (isLeaf) {
            val nb = ns.head
            g(nb) -= i
            removed += i
            nextToCheck += nb
          }
        }
        if (nextToCheck.nonEmpty) _do(nextToCheck)
      }

      _do(g.keySet)
      removed.foreach(g.remove)
    }

    private def removeIntermediateNodes(g: Graph, except: Set[Int]): Unit = {
      val removed = mutable.ArrayBuffer[Int]()
      (g.keySet -- except).foreach { i =>
        val ns = g(i).keys
        if (ns.size == 2) {
          val na = ns.head
          val nb = ns.last
          if (na != nb) {
            val w = math.min(g(na).getOrElse(nb, Int.MaxValue), math.max(g(na)(i), g(i)(nb)))
            g(na)(nb) = w
            g(nb)(na) = w
          }
          g(na) -= i
          g(nb) -= i
          g(i) = mutable.Map.empty[Int, Int]
          removed += i
        }
      }
      removed.foreach(g.remove)
    }

    def minMaxWeight(start: Int, end: Int, g: Graph): Int = {
      removeUnusedLeaves(g, Set(start, end))
      removeIntermediateNodes(g, Set(start, end))
      val sortedG = mutable.Map[Int, Seq[(Int, Int)]]()

      var minMax = Int.MaxValue
      val minWeightMap = mutable.Map[Int, Int]()

      def _do(node: Int, weight: Int): Unit = {
        minWeightMap(node) = weight
        if (node == end) {
          if (minMax > weight) minMax = weight
        } else {
          if (!sortedG.contains(node)) sortedG(node) = g(node).toSeq.sortBy(_._2)
          sortedG(node).foreach { case (nb, w) =>
            val mmw = math.max(w, weight)
            if (mmw < minMax && minWeightMap.get(nb).forall(_ > mmw)) {
              _do(nb, mmw)
            }
          }
        }
      }

      _do(1, 0)
      minMax
    }

    def main(args: Array[String]): Unit = {
      val sc = new Reader
      val n = sc.nextInt()
      val e = sc.nextInt()
      val g = mutable.Map[Int, mutable.Map[Int, Int]]()
      (1 to n).foreach(g += _ -> mutable.Map())
      (1 to e).foreach { _ =>
        val (i, j, w) = (sc.nextInt(), sc.nextInt(), sc.nextInt())
        g(i)(j) = w
        g(j)(i) = w
      }
      val res = minMaxWeight(1, n, g)
      if (res == Int.MaxValue) {
        println("NO PATH EXISTS")
      } else {
        println(res)
      }
    }

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
        if (neg) return -ret
        ret
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

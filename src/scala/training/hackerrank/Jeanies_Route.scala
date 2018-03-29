package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/jeanies-route/problem
  */
object Jeanies_Route extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable
    import scala.collection.mutable.ArrayBuffer

    type Graph = mutable.Map[Int, mutable.Map[Int, Int]]

    private def removeUnusedLeaves(used: Set[Int], g: Graph): Unit = {
      val removed = ArrayBuffer[Int]()

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

    private def removeIntermediateNodes(g: Graph): Unit = {
      val removed = ArrayBuffer[Int]()
      g.keys.foreach { i =>
        val ns = g(i).keys
        if (ns.size == 2) {
          val na = ns.head
          val nb = ns.last
          val w = g(na)(i) + g(i)(nb)
          g(na)(nb) = w
          g(nb)(na) = w
          g(na) -= i
          g(nb) -= i
          removed += i
        }
      }
      removed.foreach(g.remove)
    }

    private def calculateMinRouteWeight(g: Graph): Int = {
      var maxRouteWeight = 0
      var totalWeight = 0
      val weights = mutable.Map[Int, Seq[Int]]().withDefaultValue(Seq(0))

      def leaves = g.filter(_._2.size == 1).keys

      def accumulateAndRemove(c: Int): Unit = {
        if (g(c).isEmpty) return
        val (p, wc) = g(c).head
        val wrs = Seq(weights(p).max, weights(c).map(_ + wc).max)
        maxRouteWeight = math.max(maxRouteWeight, wrs.sum)
        totalWeight += wc
        weights(p) = wrs
        g -= c
        g(p) -= c
      }

      def _do(): Unit = {
        val ns = leaves
        if (ns.nonEmpty) {
          ns.foreach(accumulateAndRemove)
          _do()
        }
      }

      _do()
      2 * totalWeight - maxRouteWeight
    }

    private def jeanisRoute(n: Int, letters: Array[Int], g: Graph): Int = {
      val lettersSet = letters.toSet
      removeUnusedLeaves(lettersSet, g)
      removeIntermediateNodes(g)
      calculateMinRouteWeight(g)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val k = sc.nextInt()
      val letters = Array.fill(k)(sc.nextInt() - 1)
      val g = mutable.Map[Int, mutable.Map[Int, Int]]()
      (0 until n).foreach(i => g.put(i, mutable.Map()))
      (1 until n).foreach { _ =>
        val (i, j, w) = (sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt())
        g(i)(j) = w
        g(j)(i) = w
      }
      val res = jeanisRoute(n, letters, g)
      println(res)
    }
  }

}

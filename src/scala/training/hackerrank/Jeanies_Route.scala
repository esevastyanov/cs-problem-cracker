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

      def _do(toCheck: Iterable[Int]): Unit = {
        val nextToCheck = ArrayBuffer[Int]()
        toCheck.foreach { i =>
          if (!used.contains(i)) {
            val ns = g(i).keys
            val isLeaf = ns.size == 1
            if (isLeaf) {
              val nb = ns.head
              g(nb) -= i
              removed += i
              nextToCheck += nb
            }
          }
        }
        if (nextToCheck.nonEmpty) _do(nextToCheck)
      }

      _do(g.keys)
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

    private def getFullWeights(start: Int, g: Graph): mutable.Map[Int, Int] = {
      val fws = mutable.Map[Int, Int]().withDefaultValue(0)

      def _do(start: Int, prev: Int): Unit = {
        val ns = g(start).keys.filterNot(_ == prev)
        ns.foreach { nb =>
          _do(nb, start)
          fws.put(start, fws(start) + g(start)(nb) + fws(nb))
        }
      }

      _do(start, -1)
      fws
    }

    private def go(node: Int, g: Graph, fws: mutable.Map[Int, Int], limit: Int): Int = {
      def _do(node: Int, prev: Int, sum: Int): Int = {
        val ns = g(node).keys.filterNot(_ == prev)
        if (ns.isEmpty) return sum
        val nsAndWs = ns.map(nb => (nb, g(node)(nb) + fws(nb)))
        val (mn, _) = nsAndWs.maxBy(_._2)
        val partialResult = sum + nsAndWs.filterNot(_._1 == mn).map(_._2).sum * 2
        if (partialResult > limit) {
          partialResult
        } else {
          _do(mn, node, partialResult + g(node)(mn))
        }
      }

      _do(node, -1, 0)
    }

    private def jeanisRoute(n: Int, letters: Array[Int], g: Graph): Int = {
      val lettersSet = letters.toSet
      removeUnusedLeaves(lettersSet, g)
      removeIntermediateNodes(g)
      var minWeight = Int.MaxValue
      g.keys.foreach { n =>
        val result = go(n, g, getFullWeights(n, g), minWeight)
        if (result < minWeight) minWeight = result
      }
      minWeight
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

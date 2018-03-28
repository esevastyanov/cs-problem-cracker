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

    private def go(node: Int, g: Graph, fws: mutable.Map[Int, Int]): Int = {
      def _do(node: Int, prev: Int, step: Int): Int = {
        val ns = g(node).keys.filterNot(_ == prev)
        if (ns.isEmpty) return 0
        val nsAndWs = ns.map(nb => (nb, g(node)(nb) + fws(nb)))
        if (step == 0 && nsAndWs.size >= 2) {
          val (mn1, _) = nsAndWs.maxBy(_._2)
          val exceptMn1 = nsAndWs.filterNot(_._1 == mn1)
          val (mn2, _) = exceptMn1.maxBy(_._2)
          val exceptMn1And2 = exceptMn1.filterNot(_._1 == mn2)
          exceptMn1And2.map(_._2).sum * 2 + g(node)(mn1) + g(node)(mn2) + _do(mn1, node, step + 1) + _do(mn2, node, step + 1)
        } else {
          val (mn, _) = nsAndWs.maxBy(_._2)
          nsAndWs.filterNot(_._1 == mn).map(_._2).sum * 2 + g(node)(mn) + _do(mn, node, step + 1)
        }
      }

      _do(node, -1, 0)
    }

    private def jeanisRoute(n: Int, letters: Array[Int], g: Graph): Int = {
      val lettersSet = letters.toSet
      removeUnusedLeaves(lettersSet, g)
      removeIntermediateNodes(g)
      g.find(_._2.keys.size > 1).map { case (start, _) =>
        go(start, g, getFullWeights(start, g))
      }.getOrElse {
        go(g.keys.head, g, getFullWeights(g.keys.head, g))
      }
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

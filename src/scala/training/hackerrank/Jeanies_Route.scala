package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/jeanies-route/problem
  */
object Jeanies_Route extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable.ArrayBuffer

    private def neighbours(i: Int, m: Seq[Seq[Int]]) = {
      m.indices.filter(j => m(i)(j) != 0)
    }

    private def removeUnusedLeaves(used: Set[Int], m: ArrayBuffer[ArrayBuffer[Int]]): Seq[Int] = {
      val removed = ArrayBuffer[Int]()

      def _do(toCheck: Seq[Int]): Unit = {
        val nextToCheck = ArrayBuffer[Int]()
        toCheck.foreach { i =>
          if (!used.contains(i)) {
            val ns = neighbours(i, m)
            val isLeaf = ns.size == 1
            if (isLeaf) {
              val nb = ns.head
              m(i)(nb) = 0
              m(nb)(i) = 0
              nextToCheck += nb
              removed += i
            }
          }
        }
        if (nextToCheck.nonEmpty) _do(nextToCheck)
      }

      _do(m.indices)
      removed
    }

    private def removeIntermediateNodes(m: ArrayBuffer[ArrayBuffer[Int]]): Seq[Int] = {
      val removed = ArrayBuffer[Int]()
      m.indices.foreach { i =>
        val ns = neighbours(i, m)
        if (ns.size == 2) {
          val na = ns.head
          val nb = ns.last
          val w = m(na)(i) + m(i)(nb)
          m(na)(nb) = w
          m(nb)(na) = w
          m(na)(i) = 0
          m(i)(na) = 0
          m(nb)(i) = 0
          m(i)(nb) = 0
          removed += i
        }
      }
      removed
    }

    private def reduce(removed: Set[Int], m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      for (i <- m.indices if !removed.contains(i)) yield {
        for (j <- m.indices if !removed.contains(j)) yield {
          m(i)(j)
        }
      }
    }

    private def getMostValuableLeaf(m: Seq[Seq[Int]]): Int = {
      var leaf = -1
      var max = 0
      m.indices.foreach { i =>
        val ns = neighbours(i, m)
        val isLeaf = ns.size == 1
        if (isLeaf && max < m(i)(ns.head)) {
          leaf = i
          max = m(i)(ns.head)
        }
      }
      leaf
    }

    private def getFullWeights(start: Int, m: Seq[Seq[Int]]): Seq[Int] = {
      val fws = ArrayBuffer.fill(m.length)(0)

      def _do(start: Int, prev: Int): Unit = {
        val ns = neighbours(start, m).filterNot(_ == prev)
        ns.foreach { nb =>
          _do(nb, start)
          fws(start) += m(start)(nb) + fws(nb)
        }
      }

      _do(start, -1)
      fws
    }

    private def go(node: Int, m: Seq[Seq[Int]], fws: Seq[Int]): Int = {
      def _do(node: Int, prev: Int): Int = {
        val ns = neighbours(node, m).filterNot(_ == prev)
        if (ns.isEmpty) return 0
        val nsAndWs = ns.map(nb => (nb, m(node)(nb) + fws(nb)))
        val (mn, _) = nsAndWs.maxBy(_._2)
        nsAndWs.filterNot(_._1 == mn).map(_._2).sum * 2 + m(node)(mn) + _do(mn, node)
      }

      _do(node, -1)
    }

    private def jeanisRoute(n: Int, letters: Seq[Int], routes: Seq[(Int, Int, Int)]): Int = {
      val lettersSet = letters.toSet
      val g = ArrayBuffer.fill(n)(ArrayBuffer.fill(n)(0))
      routes.foreach { case (from, to, weight) =>
        g(from)(to) = weight
        g(to)(from) = weight
      }
      val removed = removeUnusedLeaves(lettersSet, g) ++ removeIntermediateNodes(g)
      val reducedGraph = reduce(removed.toSet, g)
      val mostValuableLeaf = getMostValuableLeaf(reducedGraph)
      if (mostValuableLeaf == -1) {
        /*There is only one node left. Do nothing*/
        return 0
      }
      val fullWeights = getFullWeights(mostValuableLeaf, reducedGraph)
      go(mostValuableLeaf, reducedGraph, fullWeights)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k = sc.nextInt()
      val letters = ArrayBuffer.fill(k)(sc.nextInt() - 1)
      val routes = ArrayBuffer.fill(n - 1)((sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt()))
      val res = jeanisRoute(n, letters, routes)
      println(res)
    }
  }

}

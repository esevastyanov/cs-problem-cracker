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

    private def neighbours(i: Int, m: Array[Array[Int]]) = {
      m.indices.filter(j => m(i)(j) != 0)
    }

    private def removeUnusedLeaves(used: Set[Int], m: Array[Array[Int]]): Seq[Int] = {
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

    private def removeIntermediateNodes(m: Array[Array[Int]]): Seq[Int] = {
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

    private def reduce(removed: Set[Int], m: Array[Array[Int]]): Array[Array[Int]] = {
      val reducedSize = m.length - removed.size
      val reduced = Array.fill(reducedSize)(Array.fill(reducedSize)(0))
      var ii = 0
      for (i <- m.indices) yield {
        if (!removed.contains(i)) {
          var jj = 0
          for (j <- m.indices) yield {
            if (!removed.contains(j)) {
              reduced(ii)(jj) = m(i)(j)
              jj += 1
            }
          }
          ii += 1
        }
      }
      reduced
    }

    private def getMostValuableLeaf(m: Array[Array[Int]]): Int = {
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

    private def getFullWeights(start: Int, m: Array[Array[Int]]): Seq[Int] = {
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

    private def go(node: Int, m: Array[Array[Int]], fws: Seq[Int]): Int = {
      def _do(node: Int, prev: Int, sum: Int): Int = {
        val ns = neighbours(node, m).filterNot(_ == prev)
        if (ns.isEmpty) return sum
        val nsAndWs = ns.map(nb => (nb, m(node)(nb) + fws(nb)))
        val (mn, _) = nsAndWs.maxBy(_._2)
        _do(mn, node, sum + nsAndWs.filterNot(_._1 == mn).map(_._2).sum * 2 + m(node)(mn))
      }

      _do(node, -1, 0)
    }

    private def jeanisRoute(n: Int, letters: Array[Int], g: Array[Array[Int]]): Int = {
      val lettersSet = letters.toSet
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
      val letters = Array.fill(k)(sc.nextInt() - 1)
      val g = Array.fill(n)(Array.fill(n)(0))
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

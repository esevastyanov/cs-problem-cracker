package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/cut-the-tree/problem
  */
object Cut_The_Tree extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    def cutTheTree(vertices: Array[Int], edges: Array[(Int, Int)]): Int = {
      val total = vertices.sum
      var min = Int.MaxValue
      val tree = mutable.Map[Int, mutable.Set[Int]]()
      edges.foreach { case (a, b) =>
        tree.getOrElseUpdate(a, mutable.Set[Int]()) += b
        tree.getOrElseUpdate(b, mutable.Set[Int]()) += a
      }
      var leaves = tree.filter(_._2.size == 1).keys
      while (tree.size > 2) {
        leaves = leaves.flatMap { u =>
          val weight = vertices(u)
          min = math.min(min, math.abs(total - 2 * weight))
          val v = tree(u).head
          vertices(v) += weight
          tree(v) -= u
          tree.remove(u)
          if (tree(v).size == 1) Seq(v) else Nil
        }
      }
      min
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val vertices = Array.fill(n)(sc.nextInt())
      val edges = Array.fill(n - 1)((sc.nextInt() - 1, sc.nextInt() - 1))
      println(cutTheTree(vertices, edges))
    }
  }

}

package training.hackerrank

object Matrix_Layer_Rotation extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    class Q(
      val idx: mutable.Queue[(Int, Int)] = new mutable.Queue(),
      val q: mutable.Queue[Int] = new mutable.Queue()
    )

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      sc.nextLine()
      val m = sc.nextInt
      val n = sc.nextInt
      val r = sc.nextInt

      val mx = new Array[Array[Int]](m)
      for (i <- 0 until m) {
        mx(i) = new Array[Int](n)
        for (j <- 0 until n) {
          mx(i)(j) = sc.nextInt
        }
      }

      val qqs = for (k <- 0 until math.min(m, n) / 2) yield {
        val qq = new Q()
        for (jj <- k until n - k - 1) {
          qq.idx.enqueue((k, jj))
          qq.q.enqueue(mx(k)(jj))
        }
        for (ii <- k until m - k - 1) {
          qq.idx.enqueue((ii, n - k - 1))
          qq.q.enqueue(mx(ii)(n - k - 1))
        }
        for (jj <- n - k - 1 until k by -1) {
          qq.idx.enqueue((m - k - 1, jj))
          qq.q.enqueue(mx(m - k - 1)(jj))
        }
        for (ii <- m - k - 1 until k by -1) {
          qq.idx.enqueue((ii, k))
          qq.q.enqueue(mx(ii)(k))
        }
        qq
      }

      qqs.foreach { qq =>
        val d = r % qq.q.size
        for (_ <- 1 to d) {
          qq.q.enqueue(qq.q.dequeue())
        }
        for (_ <- 0 until qq.q.size) {
          val (i, j) = qq.idx.dequeue()
          val e = qq.q.dequeue()
          mx(i)(j) = e
        }
      }

      for (i <- 0 until m) {
        for (j <- 0 until n) {
          print(mx(i)(j) + " ")
        }
        println()
      }
    }
  }

}

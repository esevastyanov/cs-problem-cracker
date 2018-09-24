package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/rust-murderer/problem
  */
object Rust_Murderer extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def rustMurdered(n: Int, s: Int, g: mutable.Map[Int, mutable.Set[Int]]): Array[Int] = {
      val r = Array.fill(n)(Int.MaxValue)

      def _do(is: mutable.Set[Int]): Unit = {
        if (is.isEmpty) return
        val nextIs = mutable.Set[Int]()
        is.foreach { i =>
          g(i).foreach { j =>
            if (r(i - 1) + 1 < r(j - 1)) {
              r(j - 1) = r(i - 1) + 1
              nextIs += j
            }
          }
        }
        _do(nextIs)
      }

      r(s - 1) = 0
      _do(mutable.Set(s))
      r
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val n = sc.nextInt()

        def newMutableSet(): mutable.Set[Int] = mutable.Set(1 to n: _*)

        val g = mutable.Map[Int, mutable.Set[Int]]((1 to n).map(_ -> newMutableSet()): _*)
        (1 to sc.nextInt()).foreach { _ =>
          val x = sc.nextInt()
          val y = sc.nextInt()
          g(x).remove(x)
          g(x).remove(y)
          g(y).remove(x)
          g(y).remove(y)
        }
        rustMurdered(n, sc.nextInt(), g).foreach(i => if (i != 0) print(i + " "))
        println()
      }
    }
  }

}

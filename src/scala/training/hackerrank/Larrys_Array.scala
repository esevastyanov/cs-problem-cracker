package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/larrys-array/problem
  */
object Larrys_Array extends App
{
  Solution.main(args)

  object Solution
  {

    def larrysArray(arr: Array[Int]): String = {
      arr.sorted
      if (arr.length == 2) {
        if (arr(0) < arr(1)) "YES" else "NO"
      } else {
        val n = arr.length
        val (_, i) = arr.zipWithIndex.find { case (e, _) => e == n }.get
        val brr = arr.filterNot(_ == n)
        if ((n - 1 - i) % 2 == 1) {
          val m = brr.length
          val t = brr(m - 1)
          brr(m - 1) = brr(m - 2)
          brr(m - 2) = t
        }
        larrysArray(brr)
      }
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      var t = sc.nextInt()
      var a0 = 0
      (1 to t).map { _ =>
        val n = sc.nextInt()
        val arr = Array.fill(n)(sc.nextInt())
        larrysArray(arr)
      }.foreach(
        println
      )
    }
  }

}

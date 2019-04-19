package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/angry-children/problem
  */
object Max_Min extends App
{
  Solution.main(args)

  object Solution
  {

    def maxMin(n: Int, k: Int, arr: Array[Int]): Int = {
      val sorted = arr.sorted
      var minMaxMin = Int.MaxValue
      sorted.indices.foreach(i =>
        minMaxMin = math.min(minMaxMin, if (i + k - 1 >= n) Int.MaxValue else sorted(i + k - 1) - sorted(i))
      )
      minMaxMin
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k = sc.nextInt()
      var arr = Array.fill(n)(sc.nextInt())
      println(maxMin(n, k, arr))
    }
  }

}

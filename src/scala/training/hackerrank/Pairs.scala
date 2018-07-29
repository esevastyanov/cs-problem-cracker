package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/pairs/problem
  */
object Pairs extends App
{
  Solution.main(args)

  object Solution
  {

    def pairs(k: Int, arr: Array[Int]): Int = {
      var i, j, s, p = 0
      while (i < arr.length - 1) {
        if (s < k && j < arr.length - 1) {
          s += arr(j + 1) - arr(j)
          j += 1
        } else {
          s -= arr(i + 1) - arr(i)
          i += 1
        }
        if (s == k) p+=1
      }
      p
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k = sc.nextInt()
      val arr = Array.fill(n)(sc.nextInt())
      println(pairs(k, arr.sorted))
    }
  }

}

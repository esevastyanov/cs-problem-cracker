package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/absolute-permutation/problem
  */
object Absolute_Permutation extends App
{
  Solution.main(args)

  object Solution
  {

    def absolutePermutation(n: Int, k: Int): Unit = {
      if (k == 0) {
        (1 to n).foreach { i => print(s"$i ") }
      } else {
        if (n % (k * 2) == 0) {
          (1 to n).foreach { i =>
            if ((i - 1) / k % 2 == 0) {
              print(s"${i + k} ")
            } else {
              print(s"${i - k} ")
            }
          }
        } else {
          print("-1")
        }
      }
      print("\n")
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val t = sc.nextInt()
      (1 to t).foreach { _ =>
        val n, k = sc.nextInt()
        absolutePermutation(n, k)
      }
    }
  }

}

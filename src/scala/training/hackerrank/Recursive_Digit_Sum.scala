package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/recursive-digit-sum/problem
  */
object Recursive_Digit_Sum extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.annotation.tailrec

    @tailrec
    def superDigit(n: Long): Int = {
      if (n < 10) {
        n.toInt
      } else {
        var sum = 0L
        var k = n
        while (k > 0) {
          sum += k % 10
          k /= 10
        }
        superDigit(sum)
      }
    }

    def digitSum(n: String, k: Int): Int = {
      var sum = 0L
      n.foreach(sum += _ - '0')
      superDigit(sum * k)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.next()
      val k = sc.nextInt()
      println(digitSum(n, k))
    }
  }

}

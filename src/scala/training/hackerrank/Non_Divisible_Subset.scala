package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/non-divisible-subset/problem
  */
object Non_Divisible_Subset extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def nonDivisibleSubset(k: Int, arr: Array[Int]): Int = {
      val m = mutable.Map[Int, Int]().withDefaultValue(0)
      arr.foreach { a => m.put(a % k, m(a % k) + 1) }
      var sum = 0
      for (i <- 1 to k / 2 - (k + 1) % 2) {
        sum += math.max(m(i), m(k - i))
      }
      sum +
        (if (m(0) > 0) 1 else 0) +
        (if (k % 2 == 0 && m(k / 2) > 0) 1 else 0)
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      var n = sc.nextInt()
      var k = sc.nextInt()
      var arr = Array.fill(n)(sc.nextInt())
      val result = nonDivisibleSubset(k, arr)
      println(result)
    }
  }

}

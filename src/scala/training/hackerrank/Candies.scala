package training.hackerrank

object Candies
{

  object Solution
  {

    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      var n = sc.nextInt()
      var arr = new Array[Int](n)
      for (arr_i <- 0 until n) {
        arr(arr_i) = sc.nextInt()
      }
      println(candies(arr))
    }

    def candies(arr: Seq[Int]): Long = {
      var cs = mutable.ArrayBuffer.fill(arr.size)(1)
      for (i <- arr.indices) {
        if (i != 0 && arr(i - 1) < arr(i)) cs(i) = cs(i - 1) + 1
      }
      for (i <- arr.indices.reverse) {
        if (i != arr.length - 1 && arr(i) > arr(i + 1) && cs(i) <= cs(i + 1)) cs(i) = cs(i + 1) + 1
      }
      val sum: Long = (0L /: cs)((s, e) => s + e)
      sum
    }
  }

}

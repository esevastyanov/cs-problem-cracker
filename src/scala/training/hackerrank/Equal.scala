package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/equal/problem
  */
object Equal
{

  object Solution
  {

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      var n = sc.nextInt()
      for (i <- 1 to n) {
        val ni = sc.nextInt()
        var arr = new Array[Int](ni)
        for (arr_i <- 0 until ni) {
          arr(arr_i) = sc.nextInt()
        }
        println(operations(arr))
      }
    }

    def operations(chocolates: Seq[Int]): Long = {
      var ops = 0L
      val min = chocolates.min
      val decreased = chocolates.map {
        e =>
          val k = (e - min) / 5
          ops += k
          e - 5 * k - min
      }
      ops + {
        val m = decreased.groupBy(identity).mapValues(_.size).withDefaultValue(0)
        Seq(
          /*+0*/ m(4) * 2 + m(3) * 2 + m(2) + m(1),
          /*+1*/ m(4) + m(3) * 2 + m(2) * 2 + m(1) + m(0),
          /*+2*/ m(4) * 2 + m(3) + m(2) * 2 + m(1) * 2 + m(0)
        ).min
      }
    }
  }

}

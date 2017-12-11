package training.hackerrank

object Richie_Rich extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      var k = sc.nextInt()
      val s = sc.next()
      val as = s.toArray
      val set = mutable.Set[Int]()
      for (i <- 0 until as.length / 2) {
        val j = as.length - 1 - i
        if (as(i) != as(j)) {
          val max = math.max(as(i), as(j))
          as(i) = max.toChar
          as(j) = max.toChar
          set += i
          k -= 1
        }
      }
      if (k < 0) {
        println(-1)
        return
      }
      for (i <- 0 until as.length / 2 if k > 0) {
        if (as(i) != '9') {
          val j = as.length - 1 - i
          if (set.contains(i) || i == j) {
            k -= 1
            as(i) = '9'
            as(j) = '9'
          } else if (k >= 2) {
            k -= 2
            as(i) = '9'
            as(j) = '9'
          }
        }
      }
      println(as.mkString(""))
    }
  }

}

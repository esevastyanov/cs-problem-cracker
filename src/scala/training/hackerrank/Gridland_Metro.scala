package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/gridland-metro/problem
  */
object Gridland_Metro extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    def gridlandMetro(n: Long, m: Long, k: Int, trains: Array[(Long, Long, Long)]): Long = {
      val trainsSorted = trains.sorted((x: (Long, Long, Long), y: (Long, Long, Long)) => {
        if (x._1 != y._1) {
          java.lang.Long.signum(x._1 - y._1)
        } else {
          if (x._2 != y._2) {
            java.lang.Long.signum(x._2 - y._2)
          } else {
            java.lang.Long.signum(x._3 - y._3)
          }
        }
      })
      val trainsQueue = mutable.Stack[(Long, Long, Long)]()
      trainsSorted.foreach(t =>
        if (trainsQueue.isEmpty || t._1 != trainsQueue.head._1 || t._2 > trainsQueue.head._3) {
          trainsQueue.push(t)
        } else if (t._3 > trainsQueue.head._3) {
          val tt = trainsQueue.pop()
          trainsQueue.push((tt._1, tt._2, t._3))
        }
      )
      var trainLength = 0L
      trainsQueue.foreach(t => trainLength += t._3 - t._2 + 1)
      n * m - trainLength
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, m = sc.nextLong()
      val k = sc.nextInt()
      val trains = Array.fill(k)((sc.nextLong(), sc.nextLong(), sc.nextLong()))
      println(gridlandMetro(n, m, k, trains))
    }
  }

}

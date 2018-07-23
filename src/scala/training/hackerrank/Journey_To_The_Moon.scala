package training.hackerrank


/**
  * https://www.hackerrank.com/challenges/journey-to-the-moon/problem
  */
object Journey_To_The_Moon extends App
{
  Solution.main(args)

  object Solution
  {
    import scala.collection.mutable

    def journeyToTheMoon(n: Int, pairs: Array[(Int, Int)]): Long = {
      val m = mutable.Map[Int, mutable.Set[Int]]()
      pairs.foreach { case (p1, p2) =>
        m.getOrElseUpdate(p1, mutable.Set.empty[Int]) += p2
        m.getOrElseUpdate(p2, mutable.Set.empty[Int]) += p1
      }

      val population = (0 until n).map { i =>
        m.get(i).map(s =>
          if (s.isEmpty) 0
          else {
            var k = 1
            var nonVisited = m.getOrElse(i, mutable.Set.empty[Int])
            m(i) = mutable.Set.empty[Int]
            while (nonVisited.nonEmpty) {
              val visitNext = mutable.Set[Int]()
              nonVisited.foreach { j =>
                m.get(j).foreach { ss =>
                  if (ss.nonEmpty) {
                    k += 1
                    visitNext ++= ss
                  }
                }
                m(j) = mutable.Set.empty[Int]
              }
              nonVisited = visitNext
            }
            k
          }
        ).getOrElse(1)
      }.filterNot(_ == 0).toArray

      var res = 0L
      for (i <- population.indices) {
        for (j <- i + 1 until population.length) {
          res += population(i) * population(j)
        }
      }
      res
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, p = sc.nextInt()
      val pairs = Array.fill(p)((sc.nextInt(), sc.nextInt()))
      println(journeyToTheMoon(n, pairs))
    }
  }

}

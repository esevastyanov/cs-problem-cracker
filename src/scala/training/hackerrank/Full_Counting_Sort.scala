package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/countingsort4/problem
  */
object Full_Counting_Sort extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable.ListBuffer

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val a = Array.fill(sc.nextInt())((sc.nextInt(), sc.next()))
      val m = Array.fill(100)(ListBuffer[String]())
      for (j <- a.indices) {
        val (i, s) = if (j >= a.length / 2) a(j) else a(j).copy(_2 = "-")
        m(i) += s
      }
      m.foreach { l =>
        if (l.nonEmpty) print(l.mkString(" ") + " ")
      }
    }
  }

}

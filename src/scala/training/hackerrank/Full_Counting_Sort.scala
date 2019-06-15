package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/countingsort4/problem
  */
object Full_Counting_Sort extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val a = Array.fill(sc.nextInt())((sc.nextInt(), sc.next()))
      println(
        a.zipWithIndex
          .map { case ((i, s), j) => if (j >= a.length / 2) (i, s) else (i, "-") }
          .sortBy(_._1)
          .map(_._2)
          .mkString(" ")
      )
    }
  }

}

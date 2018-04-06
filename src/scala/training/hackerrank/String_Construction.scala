package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/string-construction/problem
  */
object String_Construction extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        println(sc.next().groupBy(identity).values.size)
      }
    }
  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/climbing-the-leaderboard/problem
  */
object Climbing_The_Leaderboard extends App
{

  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def normalizeScores(scores: Array[Int]): Array[Int] = {
      val queue = new mutable.Queue[Int]()
      scores.foreach { s => if (queue.isEmpty || s != queue.last) queue += s }
      queue.toArray
    }

    def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
      val nScores = normalizeScores(scores)
      var position = nScores.length - 1
      alice.map { a =>
        if (position < 0) {
          1
        } else {
          while (position >= 0 && nScores(position) <= a) {
            position -= 1
          }
          position + 2
        }
      }
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val scores = Array.fill(n)(sc.nextInt())
      val m = sc.nextInt()
      val alice = Array.fill(m)(sc.nextInt())
      val result = climbingLeaderboard(scores, alice)
      println(result.mkString("\n"))
    }
  }

}

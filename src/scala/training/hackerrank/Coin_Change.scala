package training.hackerrank

object Coin_Change extends App
{

  Solution.main(args)

  object Solution
  {
    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val m = sc.nextInt()
      val coins = Seq.fill(m)(sc.nextInt()).distinct.sorted
      println(change(n, coins.toSet.toSeq.sorted))
    }

    def change(n: Int, coins: Seq[Int]): Long = {
      val ch = Array.fill[Long](n+1)(0)
      ch(0) = 1
      coins.foreach { c =>
        for (i <- c to n) {
          ch(i) += ch(i - c)
        }
      }
      ch(n)
    }
  }
}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/hackerland-radio-transmitters/problem
  */
object Hackerland_Radio_Transmitters extends App
{
  Solution.main(args)

  object Solution
  {

    def hackerlandRadioTransmitters(x: Array[Int], k: Int): Int = {
      var i = 0
      var j = 0
      var r = 0
      while (j < x.length) {
        if (x(j) - k <= x(i)) j += 1
        else {
          r += 1
          val j1 = j - 1
          while (j < x.length && x(j1) + k >= x(j)) j += 1
          i = j
        }
      }
      if (i < x.length) r += 1
      r
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n, k = sc.nextInt()
      val x = Array.fill(n)(sc.nextInt())
      println(hackerlandRadioTransmitters(x.sorted, k))
    }
  }

}

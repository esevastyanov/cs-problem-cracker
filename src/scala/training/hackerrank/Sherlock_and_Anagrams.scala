package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/sherlock-and-anagrams/problem
  */
object Sherlock_and_Anagrams extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable
    import scala.collection.mutable.ListBuffer

    def numberOfAnagrams(s: String): Int = {

      def isAnagram(t1: (Int, Int), t2: (Int, Int)): Boolean = {
        val m = new Array[Int]('z' - 'a' + 1)
        (t1._1 to t1._2).foreach { i => m(s(i) - 'a') += 1 }
        (t2._1 to t2._2).foreach { i => m(s(i) - 'a') -= 1 }
        m.forall(_ == 0)
      }

      val m = mutable.Map[(Int, Int, Int), ListBuffer[(Int, Int)]]()
      s.indices.foreach { i =>
        var sum = 0
        var bitSum = 0
        (i until s.length).foreach { j =>
          sum += s(j)
          bitSum ^= s(j)
          m.getOrElseUpdate((sum, bitSum, j - i), ListBuffer.empty) += ((i, j))
        }
      }
      var numberOfAnagrams = 0
      m.values.foreach { l =>
        l.indices.foreach { i =>
          (i + 1 until l.size).foreach { j =>
            numberOfAnagrams += (if (isAnagram(l(i), l(j))) 1 else 0)
          }
        }
      }
      numberOfAnagrams
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        println(numberOfAnagrams(sc.next()))
      }
    }
  }

}

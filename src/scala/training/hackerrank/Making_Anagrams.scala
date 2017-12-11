package training.hackerrank

object Making_Anagrams extends App
{
  Solution.main(args)

  object Solution {

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val a = sc.next()
      val b = sc.next()
      val as = a.groupBy(identity).mapValues(_.length).withDefaultValue(0)
      val bs = b.groupBy(identity).mapValues(_.length).withDefaultValue(0)
      val cmn = as.keys.toSeq.map(k => math.min(as(k), bs(k))).sum
      val toRemove = a.length + b.length - 2 * cmn
      println(toRemove)
    }
  }
}

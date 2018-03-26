package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/lilys-homework/problem
  */
object Lilys_Homework extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    private def permutationNumber(arr: Array[Int], srr: Array[Int]) = {
      val arm = mutable.Map[Int, Int]()
      val srm = mutable.Map[Int, Int]()
      for (i <- arr.indices) {
        arm += (arr(i) -> i)
        srm += (srr(i) -> i)
      }
      var permutations = 0
      srm.foreach { case (se, i) =>
        val ae = arr(i)
        if (ae != se) {
          permutations += 1
          val j = arm(se)
          arm.update(ae, j)
          arm.update(se, i)
          arr(i) = se
          arr(j) = ae
        }
      }
      permutations
    }

    def lilysHomework(arr: Array[Int]): Int = {
      val srr = arr.sorted
      val arr1 = arr
      val arr2 = new Array[Int](arr.length)
      arr.copyToArray(arr2)
      math.min(
        permutationNumber(arr1, srr),
        permutationNumber(arr2, srr.reverse)
      )
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      var n = sc.nextInt()
      var arr = Array.fill(n)(sc.nextInt())
      val result = lilysHomework(arr)
      println(result)
    }
  }

}

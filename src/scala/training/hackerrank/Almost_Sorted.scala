package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/almost-sorted/problem
  */
object Almost_Sorted extends App
{
  Solution.main(args)

  object Solution
  {

    def isSorted(arr: Array[Int]): Boolean = {
      arr.isEmpty || arr.length == 1 || {
        var prev = arr.head
        arr.tail.forall { e =>
          val res = e - prev > 0
          prev = e
          res
        }
      }
    }

    def swapToSort(arr: Array[Int], es: (Int, Int)): Boolean = {
      val (i, j) = es
      val brr = new Array[Int](arr.length)
      arr.copyToArray(brr)
      val t = brr(i)
      brr(i) = brr(j)
      brr(j) = t
      isSorted(brr)
    }

    def reverseToSort(arr: Array[Int], es: (Int, Int)): Boolean = {
      val (i, j) = es
      val brr = new Array[Int](arr.length)
      for (k <- brr.indices) {
        brr(k) = {
          if (k < i || k > j) {
            arr(k)
          } else {
            arr(j - (k - i))
          }
        }
      }
      isSorted(brr)
    }

    def extremums(arr: Array[Int]): Seq[Int] = {
      arr.indices.flatMap { i =>
        val left = if (i > 0) Some(arr(i - 1)) else None
        val right = if (i < arr.length - 1) Some(arr(i + 1)) else None
        val max = i != arr.length - 1 && left.forall(_ < arr(i)) && right.forall(_ < arr(i))
        val min = i != 0 && left.forall(_ > arr(i)) && right.forall(_ > arr(i))
        if (max || min) Some(i) else None
      }
    }

    def almostSorted(arr: Array[Int]): Unit = {
      if (isSorted(arr)) {
        println("yes")
        return
      }
      val es = extremums(arr)
      val (i, j) = (es.head, es.last)
      if (swapToSort(arr, (i, j))) {
        println("yes")
        println(s"swap ${i+1} ${j+1}")
        return
      }
      if (reverseToSort(arr, (i, j))) {
        println("yes")
        println(s"reverse ${i+1} ${j+1}")
        return
      }
      println("no")
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val arr = Array.fill(n)(sc.nextInt())
      almostSorted(arr)
    }
  }

}

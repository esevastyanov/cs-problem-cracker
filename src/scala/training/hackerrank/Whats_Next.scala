package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/whats-next/problem
  */
object Whats_Next extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable.ListBuffer

    def whatsNext(arr: ListBuffer[Long]): ListBuffer[Long] = {
      val i = (arr.length - 1) / 2 * 2
      var before = arr.slice(0, i)
      before =
        if (before.nonEmpty) {
          before(before.length - 1) -= 1
          if (before.last == 0) {
            before = before.slice(0, before.length - 1)
            if (before.nonEmpty) {
              before(before.length -1) += 1
              before ++ ListBuffer(1L)
            } else {
              ListBuffer(1L, 1L)
            }
          } else {
            before ++ ListBuffer(1L, 1L)
          }
        } else {
          ListBuffer(1L, 1L)
        }
      if (arr.length % 2 > 0) {
        var after = arr.last
        after -= 1
        before ++ (if (after > 0) ListBuffer(after) else Nil)
      } else {
        var after = arr.slice(i, arr.length) // 2 elements
        before(before.length - 1) += after.last
        before ++ (if (after.head > 1) ListBuffer(after.head - 1) else Nil)
      }
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      (1 to sc.nextInt()).foreach { _ =>
        val res = whatsNext(ListBuffer.fill(sc.nextInt())(sc.nextLong()))
        println(res.length)
        println(res.mkString(" "))
      }
    }
  }

}

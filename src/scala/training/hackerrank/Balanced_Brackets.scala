package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/ctci-balanced-brackets/problem
  */
object Balanced_Brackets extends App
{
  Solution.main(args)

  object Solution {
    import scala.collection.mutable

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      var t = sc.nextInt()
      var a0 = 0
      (1 to t).foreach { _ =>
        if (isBracketsBalanced(sc.next())) println("YES")
        else println("NO")
      }
    }

    private def isBracketsBalanced(exp: String): Boolean = {
      val s = mutable.Stack[Char]()
      exp.foreach { e =>
        if (s.isEmpty) s.push(e)
        else {
          val p = s.head
          (p, e) match {
            case ('(', ')') => s.pop()
            case ('{', '}') => s.pop()
            case ('[', ']') => s.pop()
            case _ => s.push(e)
          }
        }
      }
      s.isEmpty
    }
  }
}

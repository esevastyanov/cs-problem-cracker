package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/the-time-in-words/problem
  */
object The_Time_In_Words extends App
{
  Solution.main(args)

  object Solution
  {
    def firstDigit(d: Int): String = d match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case 5 => "five"
      case 6 => "six"
      case 7 => "seven"
      case 8 => "eight"
      case 9 => "nine"
    }

    def secondDigit(d: Int): String = d match {
      case 2 => "twenty"
      case 3 => "thirty"
      case 4 => "forty"
      case 5 => "fifty"
    }

    def twoDigits(dd: Int): String = dd match {
      case 10 => "ten"
      case 11 => "eleven"
      case 12 => "twelve"
      case 13 => "thirteen"
      case 14 => "fourteen"
      case 15 => "fifteen"
      case 16 => "sixteen"
      case 17 => "seventeen"
      case 18 => "eighteen"
      case 19 => "nineteen"
      case _ =>
        if (dd < 10) {
          firstDigit(dd)
        } else if (dd % 10 == 0){
          s"${secondDigit(dd / 10)}"
        } else {
          s"${secondDigit(dd / 10)} ${firstDigit(dd % 10)}"
        }
    }

    def minutes(m: Int): String = {
      if (m == 1) {
        "one minute"
      } else if (m == 15 || m == 45) {
        "quarter"
      } else if (m == 30) {
        "half"
      } else {
        s"${twoDigits(m)} minutes"
      }
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val h, m = sc.nextInt()
      if (m == 0) {
        println(s"${twoDigits(h)} o' clock")
      } else if (m <= 30) {
        println(s"${minutes(m)} past ${twoDigits(h)}")
      } else {
        println(s"${minutes(60 - m)} to ${twoDigits(if (h < 12) h + 1 else 1)}")
      }
    }
  }

}

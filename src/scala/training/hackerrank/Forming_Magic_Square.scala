package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/magic-square-forming/problem
  */
object Forming_Magic_Square extends App
{

  Solution.main(args)

  object Solution
  {
    type Matrix = Array[Array[Int]]

    val model = Array(
      Array(4, 9, 2),
      Array(3, 5, 7),
      Array(8, 1, 6)
    )

    def formingMagicSquare(m: Matrix): Int = {
      var minDistance = Int.MaxValue
      for (mv1 <- Seq(None, Some(Rotation90), Some(Rotation180), Some(Rotation270))) {
        for (mv2 <- Seq(None, Some(MirrorHorizontally))) {
          for (mv3 <- Seq(None, Some(MirrorDiagonally))) {
            var r = model
            r = mv1.map(_.move(r)).getOrElse(r)
            r = mv2.map(_.move(r)).getOrElse(r)
            r = mv3.map(_.move(r)).getOrElse(r)
            minDistance = math.min(distance(m, r), minDistance)
          }
        }
      }
      minDistance
    }

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val s = Array.ofDim[Int](3, 3)
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          s(i)(j) = sc.nextInt()
        }
      }
      val result = formingMagicSquare(s)
      println(result)
    }

    def distance(m1: Matrix, m2: Matrix): Int = {
      var sum = 0
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          sum += math.abs(m1(i)(j) - m2(i)(j))
        }
      }
      sum
    }

    trait Move {
      def move(m: Matrix): Matrix
    }

    object Rotation90 extends Move {
      override def move(m: Matrix): Matrix = {
        val r = Array.ofDim[Int](3, 3)
        r(0)(0) = m(0)(2)
        r(0)(1) = m(1)(2)
        r(0)(2) = m(2)(2)
        r(1)(0) = m(0)(1)
        r(1)(1) = m(1)(1)
        r(1)(2) = m(2)(1)
        r(2)(0) = m(0)(0)
        r(2)(1) = m(1)(0)
        r(2)(2) = m(2)(0)
        r
      }
    }

    object Rotation180 extends Move {
      override def move(m: Matrix): Matrix = {
        Rotation90.move(Rotation90.move(m))
      }
    }

    object Rotation270 extends Move {
      override def move(m: Matrix): Matrix = {
        Rotation90.move(Rotation180.move(m))
      }
    }

    object MirrorHorizontally extends Move {
      override def move(m: Matrix): Matrix = {
        val r = Array.ofDim[Int](3, 3)
        r(0) = m(2)
        r(1) = m(1)
        r(2) = m(0)
        r
      }
    }

    object MirrorDiagonally extends Move {
      override def move(m: Matrix): Matrix = {
        val r = Array.ofDim[Int](3, 3)
        for (i <- 0 until 3) {
          for (j <- 0 until 3) {
            r(i)(j) = m(j)(i)
          }
        }
        r
      }
    }
  }
}

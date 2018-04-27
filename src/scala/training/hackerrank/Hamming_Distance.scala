package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/hamming-distance/problem
  */
object Hamming_Distance extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val s = sc.next()
      val ba = BitArray.fill(s)
      (1 to sc.nextInt()).foreach { i =>
        sc.next() match {
          case "C" => ba.const(sc.nextInt() - 1, sc.nextInt() - 1, sc.next().head)
          case "S" => ba.swap(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt() - 1)
          case "R" => ba.reverse(sc.nextInt() - 1, sc.nextInt() - 1)
          case "W" => println(ba.toString(sc.nextInt() - 1, sc.nextInt() - 1))
          case "H" => println(ba.hamming(sc.nextInt() - 1, sc.nextInt() - 1, sc.nextInt()))
        }
      }
    }

    class BitArray(val n: Int)
    {

      import BitArray._

      val arr = new Array[Int](index(n))

      def at(i: Int): Int = {
        val na = i / SIZE
        val ne = i % SIZE
        arr(na) >> (SIZE - 1 - ne) & 1
      }

      def assign(k: Int, ch: Char): Unit = {
        assign(k, ch - 'a')
      }

      def assign(k: Int, v: Int): Unit = {
        val i = k / SIZE
        val s = k % SIZE
        arr(i) &= ~(1 << (SIZE - 1 - s))
        arr(i) |= v << (SIZE - 1 - s)
      }

      def const(l: Int, r: Int, ch: Char): Unit = {
        (l to r).foreach { i => assign(i, ch) }
      }

      def copy(l: Int, r: Int): BitArray = {
        val ba = new BitArray(r - l + 1)
        (l to r).foreach(i => ba.assign(i - l, this.at(i)))
        ba
      }

      def swap(l1: Int, r1: Int, l2: Int, r2: Int): Unit = {
        val n1 = r1 - l1 + 1
        val n2 = r2 - l2 + 1
        val b = l2 - r1 - 1
        if (n1 >= n2) {
          val ba1 = this.copy(l1, r1)
          (0 until n2).foreach(i => this.assign(l1 + i, this.at(l2 + i)))
          (0 until b).foreach(i => this.assign(l1 + n2 + i, this.at(r1 + 1 + i)))
          (0 until n1).foreach(i => this.assign(r2 - n1 + 1 + i, ba1.at(i)))
        } else {
          val ba2 = this.copy(l2, r2)
          (0 until n1).foreach(i => this.assign(r2 - i, this.at(r1 - i)))
          (0 until b).foreach(i => this.assign(r2 - n1 - i, this.at(l2 - 1 - i)))
          (0 until n2).foreach(i => this.assign(l1 + i, ba2.at(i)))
        }
      }

      def reverse(l: Int, r: Int): Unit = {
        (0 to (r - l) / 2).foreach { i =>
          if (l + i < r - i) {
            val t = this.at(l + i)
            this.assign(l + i, this.at(r - i))
            this.assign(r - i, t)
          }
        }
      }

      def toString(l: Int, r: Int): String = {
        (l to r).map(i => ('a' + at(i)).toChar).mkString
      }

      override def toString: String = {
        (0 until n).map(i => ('a' + at(i)).toChar).mkString
      }

      def hamming(l1: Int, l2: Int, len: Int): Int = {
        (0 until len).map(i => math.abs(this.at(l1 + i) - this.at(l2 + i))).sum
      }

    }

    object BitArray
    {
      val SIZE = 32

      def fill(s: String): BitArray = {
        val ba = new BitArray(s.length)
        s.indices.foreach { i => ba.assign(i, s(i)) }
        ba
      }

      def index(n: Int): Int = {
        n / SIZE + math.signum(n % SIZE)
      }
    }

  }

}

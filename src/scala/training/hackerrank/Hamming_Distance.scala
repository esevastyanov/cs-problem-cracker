package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/hamming-distance/problem
  */
object Hamming_Distance extends App
{
  Solution.main(args)

  object Solution
  {
    //  TODO: Replace all / and % operators

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

      val arr : Array[Int] = new Array[Int](index(n))
      val ZERO: Int        = 0
      val ONE : Int        = ~0

      def at(i: Int): Boolean = {
        val na = i / SIZE
        val ne = i % SIZE
        (arr(na) >>> (SIZE - 1 - ne) & 1) == 0
      }

      def assign(k: Int, ch: Char): Unit = {
        assign(k, ch == 'a')
      }

      def assign(k: Int, zero: Boolean): Unit = {
        val i = k / SIZE
        val s = k % SIZE
        arr(i) &= ~(1 << (SIZE - 1 - s))
        arr(i) |= (if (zero) 0 else 1) << (SIZE - 1 - s)
      }

      def const(l: Int, r: Int, ch: Char): Unit = {
        const(l, r, ch == 'a')
      }

      def const(l: Int, r: Int, zero: Boolean): Unit = {
        if (r < l) return

        def constBlock(n: Int): Unit = {
          if (zero) arr(n) = 0 else arr(n) = ONE
        }

        def constBlockRight(i: Int): Unit = {
          val pattern = ONE << (SIZE - i % SIZE)
          if (zero) arr(i / SIZE) &= pattern else arr(i / SIZE) |= ~pattern
        }

        def constBlockLeft(i: Int): Unit = {
          val pattern = ONE << (SIZE - i % SIZE - 1)
          if (zero) arr(i / SIZE) &= ~pattern else arr(i / SIZE) |= pattern
        }

        def constBlockMiddle(l: Int, r: Int): Unit = {
          val pattern = ONE << (SIZE - l % SIZE) | (ONE >>> (r % SIZE + 1))
          if (zero) arr(l / SIZE) &= pattern else arr(l / SIZE) |= ~pattern
        }

        if (l / SIZE != r / SIZE) {
          if (l % SIZE == 0) constBlock(l / SIZE) else constBlockRight(l)
          (l / SIZE + 1 until r / SIZE).foreach(constBlock)
          if ((r + 1) % SIZE == 0) constBlock(r / SIZE) else constBlockLeft(r)
        } else {
          if (l % SIZE == 0 && (r + 1) % SIZE == 0) {
            constBlock(l / SIZE)
          } else if (l % SIZE == 0) {
            constBlockLeft(r)
          } else if ((r + 1) % SIZE == 0) {
            constBlockRight(l)
          } else {
            constBlockMiddle(l, r)
          }
        }
      }

      def shiftLeft(k: Int): Unit = {
        if (k != 0) {
          val d = k / SIZE
          if (d != 0) {
            arr.indices.foreach { i => if (i + d < arr.length) arr(i) = arr(i + d) }
            (arr.length - d until arr.length).foreach(arr(_) = 0)
          }
          val sh = k % SIZE
          if (sh != 0) {
            arr.indices.foreach { i =>
              arr(i) <<= sh
              if (i + 1 < arr.length) {
                arr(i) |= arr(i + 1) >>> SIZE - sh
              }
            }
          }
        }
      }

      def copyRaw(l: Int, r: Int): BitArray = {
        val ba = new BitArray((r / SIZE - l / SIZE + 1) * SIZE)
        (l / SIZE to r / SIZE).foreach(i => ba.arr(i - l / SIZE) = arr(i))
        ba.const(0, l % SIZE - 1, true)
        ba.const(l % SIZE + r - l + 1, (r / SIZE - l / SIZE + 1) * SIZE - 1, true)
        ba
      }

      def copy(l: Int, r: Int): BitArray = {
        val ba: BitArray = copyRaw(l, r)
        ba.shiftLeft(l % SIZE)
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
        (l to r).map(i => if (at(i)) 'a' else 'b').mkString
      }

      override def toString: String = {
        (0 until n).map(i => if (at(i)) 'a' else 'b').mkString
      }

      def hamming(l1: Int, l2: Int, len: Int): Int = {
        val ba1 = this.copyRaw(l1, l1 + len - 1)
        val ba2 = this.copyRaw(l2, l2 + len - 1)
        if (l1 % SIZE > l2 % SIZE) ba1.shiftLeft(l1 % SIZE - l2 % SIZE)
        if (l2 % SIZE > l1 % SIZE) ba2.shiftLeft(l2 % SIZE - l1 % SIZE)
        var hd = 0
        (0 until math.min(ba1.arr.length, ba2.arr.length)).foreach { i =>
          val x = ba1.arr(i) ^ ba2.arr(i)
          if (x != 0) {
            (0 until SIZE).foreach { j => hd += ((x >> j) & 1) }
          }
        }
        hd
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

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
      val _ = sc.nextInt()
      val s = sc.next()
      val ba = BitArray.fill(s)
      (1 to sc.nextInt()).foreach { _ =>
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

      type T = Int

      val arr  : Array[Int] = new Array[Int](((n - 1) >> POW) + 2)
      val M_ONE: Int        = ~0
      val ZERO : Int        = 0
      val ONE  : Int        = 1
      val MOD  : Int        = ~(M_ONE << POW)

      def at(i: Int): Boolean = {
        val na = i >> POW
        val ne = i & MOD
        (arr(na) >>> (SIZE - 1 - ne) & ONE) == 0
      }

      def assign(k: Int, ch: Char): Unit = {
        assign(k, ch == 'a')
      }

      def assign(k: Int, zero: Boolean): Unit = {
        val i = k >> POW
        val s = k & MOD
        arr(i) &= ~(ONE << (SIZE - 1 - s))
        arr(i) |= (if (zero) ZERO else ONE) << (SIZE - 1 - s)
      }

      def const(l: Int, r: Int, ch: Char): Unit = {
        const(l, r, ch == 'a')
      }

      def const(l: Int, r: Int, zero: Boolean): Unit = {
        if (r < l) return

        def constBlock(n: Int): Unit = {
          if (zero) arr(n) = ZERO else arr(n) = M_ONE
        }

        def constBlockRight(i: Int): Unit = {
          val pattern = M_ONE << (SIZE - (i & MOD))
          if (zero) arr(i >> POW) &= pattern else arr(i >> POW) |= ~pattern
        }

        def constBlockLeft(i: Int): Unit = {
          val pattern = M_ONE << (SIZE - (i & MOD) - 1)
          if (zero) arr(i >> POW) &= ~pattern else arr(i >> POW) |= pattern
        }

        def constBlockMiddle(l: Int, r: Int): Unit = {
          val pattern = (M_ONE << (SIZE - (l & MOD))) | (M_ONE >>> ((r & MOD) + 1))
          if (zero) arr(l >> POW) &= pattern else arr(l >> POW) |= ~pattern
        }

        if ((l >> POW) != (r >> POW)) {
          if ((l & MOD) == 0) constBlock(l >> POW) else constBlockRight(l)
          ((l >> POW) + 1 until r >> POW).foreach(constBlock)
          if (((r + 1) & MOD) == 0) constBlock(r >> POW) else constBlockLeft(r)
        } else {
          if ((l & MOD) == 0 && ((r + 1) & MOD) == 0) {
            constBlock(l >> POW)
          } else if ((l & MOD) == 0) {
            constBlockLeft(r)
          } else if (((r + 1) & MOD) == 0) {
            constBlockRight(l)
          } else {
            constBlockMiddle(l, r)
          }
        }
      }

      def shiftLeft(k: Int): Unit = {
        if (k != 0) {
          val d = k >> POW
          if (d != 0) {
            arr.indices.foreach { i => if (i + d < arr.length) arr(i) = arr(i + d) }
            (arr.length - d until arr.length).foreach(arr(_) = 0)
          }
          val sh = k & MOD
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

      def shiftRight(k: Int): Unit = {
        if (k != 0) {
          val d = k >> POW
          if (d != 0) {
            arr.indices.reverse.foreach { i => // TODO: Reverse
              if (i - d >= 0) arr(i) = arr(i - d) else arr(i) = 0
            }
          }
          val sh = k & MOD
          if (sh != 0) {
            arr.indices.reverse.foreach { i => // TODO: Reverse
              arr(i) >>>= sh
              if (i - 1 >= 0) {
                arr(i) |= arr(i - 1) << SIZE - sh
              }
            }
          }
        }
      }

      def copyRaw(l: Int, r: Int): BitArray = {
        val ba = new BitArray(((r >> POW) - (l >> POW) + 1) * SIZE)
        (l >> POW to r >> POW).foreach(i => ba.arr(i - (l >> POW)) = arr(i))
        ba.const(0, (l & MOD) - 1, zero = true)
        ba.const((l & MOD) + r - l + 1, ((r >> POW) - (l >> POW) + 1) * SIZE - 1, zero = true)
        ba
      }

      def copy(l: Int, r: Int): BitArray = {
        val ba: BitArray = copyRaw(l, r)
        ba.shiftLeft(l & MOD)
        ba
      }

      def swap(l1: Int, r1: Int, l2: Int, r2: Int): Unit = {
        val ba1 = this.copyRaw(0, l1 - 1)
        val ba1Len = l1
        val ba2 = this.copyRaw(l1, r1)
        val ba2Len = r1 - l1 + 1
        val ba3 = this.copyRaw(r1 + 1, l2 - 1)
        val ba3Len = l2 - r1 - 1
        val ba4 = this.copyRaw(l2, r2)
        val ba4Len = r2 - l2 + 1
        val ba5 = this.copyRaw(r2 + 1, n - 1)

        def align(ba: BitArray, s1: Int, s2: Int): Unit = {
          if ((s2 & MOD) > (s1 & MOD)) ba.shiftLeft((s2 & MOD) - (s1 & MOD))
          if ((s2 & MOD) < (s1 & MOD)) ba.shiftRight((s1 & MOD) - (s2 & MOD))
        }

        align(ba4, ba1Len, l2)
        align(ba3, ba1Len + ba4Len, r1 + 1)
        align(ba2, ba1Len + ba4Len + ba3Len, l1)
        align(ba5, ba1Len + ba4Len + ba3Len + ba2Len, r2 + 1)

        var i1, i2, i3, i4, i5 = 0
        for (i <- 0 to (n - 1) >> POW) {
          arr(i) = 0
          if (i <= ((ba1Len - 1) >> POW) && i1 < ba1.arr.length) {
            arr(i) |= ba1.arr(i1)
            i1 += 1
          }
          if (ba1Len >> POW <= i && i <= ((ba1Len + ba4Len - 1) >> POW) && i4 < ba4.arr.length) {
            arr(i) |= ba4.arr(i4)
            i4 += 1
          }
          if ((ba1Len + ba4Len) >> POW <= i && i <= ((ba1Len + ba4Len + ba3Len - 1) >> POW) && i3 < ba3.arr.length) {
            arr(i) |= ba3.arr(i3)
            i3 += 1
          }
          if (
            (ba1Len + ba4Len + ba3Len) >> POW <= i &&
              i <= ((ba1Len + ba4Len + ba3Len + ba2Len - 1) >> POW) &&
              i2 < ba2.arr.length) {
            arr(i) |= ba2.arr(i2)
            i2 += 1
          }
          if ((ba1Len + ba4Len + ba3Len + ba2Len) >> POW <= i && i5 < ba5.arr.length) {
            arr(i) |= ba5.arr(i5)
            i5 += 1
          }
        }
      }

      def reverse(l: Int, r: Int): Unit = {
        (0 to (r - l) >> 1).foreach { i =>
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

      def hamming(l1: Int, l2: Int, len: Int): Long = {
        val ba1 = this.copyRaw(l1, l1 + len - 1)
        val ba2 = this.copyRaw(l2, l2 + len - 1)
        if ((l1 & MOD) > (l2 & MOD)) ba1.shiftLeft((l1 & MOD) - (l2 & MOD))
        if ((l2 & MOD) > (l1 & MOD)) ba2.shiftLeft((l2 & MOD) - (l1 & MOD))
        var hd = 0L
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
      val POW = 5
      val SIZE = 32

      def fill(s: String): BitArray = {
        val ba = new BitArray(s.length)
        s.indices.foreach { i => ba.assign(i, s(i)) }
        ba
      }
    }

  }

}

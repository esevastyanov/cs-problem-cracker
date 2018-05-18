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

      type T = Long

      val arr  : Array[T] = new Array[T](((n - 1) >> POW) + 2)
      val rarr : Array[T] = new Array[T](((n - 1) >> POW) + 2)
      val M_ONE: T        = ~0
      val ZERO : T        = 0
      val ONE  : T        = 1
      val MOD  : Int      = ~(~0 << POW)
      val last : Int      = ((n - 1) >> POW) + 1
      val lastI: Int      = (last + 1) << POW

      def revI(i: Int): Int = lastI - i - 1

      def revN(n: Int): Int = last - n

      def const(l: Int, r: Int, ch: Char): Unit = {
        const(l, r, ch == 'a')
      }

      def const(l: Int, r: Int, zero: Boolean): Unit = {
        if (r < l) return

        def constBlock(n: Int, a: Array[T]): Unit = {
          if (zero) a(n) = ZERO else a(n) = M_ONE
        }

        def constBlockRight(i: Int, a: Array[T]): Unit = {
          val pattern = M_ONE << (SIZE - (i & MOD))
          if (zero) a(i >> POW) &= pattern else a(i >> POW) |= ~pattern
        }

        def constBlockLeft(i: Int, a: Array[T]): Unit = {
          val pattern = M_ONE << (SIZE - (i & MOD) - 1)
          if (zero) a(i >> POW) &= ~pattern else a(i >> POW) |= pattern
        }

        def constBlockMiddle(l: Int, r: Int, a: Array[T]): Unit = {
          val pattern = (M_ONE << (SIZE - (l & MOD))) | (M_ONE >>> ((r & MOD) + 1))
          if (zero) a(l >> POW) &= pattern else a(l >> POW) |= ~pattern
        }

        if ((l >> POW) != (r >> POW)) {
          if ((l & MOD) == 0) {
            constBlock(l >> POW, arr)
            constBlock(revN(l >> POW), rarr)
          } else {
            constBlockRight(l, arr)
            constBlockLeft(revI(l), rarr)
          }
          ((l >> POW) + 1 until r >> POW).foreach { i =>
            constBlock(i, arr)
            constBlock(revN(i), rarr)
          }
          if (((r + 1) & MOD) == 0) {
            constBlock(r >> POW, arr)
            constBlock(revN(r >> POW), rarr)
          } else {
            constBlockLeft(r, arr)
            constBlockRight(revI(r), rarr)
          }
        } else {
          if ((l & MOD) == 0 && ((r + 1) & MOD) == 0) {
            constBlock(l >> POW, arr)
            constBlock(revN(l >> POW), rarr)
          } else if ((l & MOD) == 0) {
            constBlockLeft(r, arr)
            constBlockRight(revI(r), rarr)
          } else if (((r + 1) & MOD) == 0) {
            constBlockRight(l, arr)
            constBlockLeft(revI(l), rarr)
          } else {
            constBlockMiddle(l, r, arr)
            constBlockMiddle(revI(r), revI(l), rarr)
          }
        }
      }

      def shiftLeft(k: Int): Unit = {
        shiftLeft(k, arr)
        shiftRight(k, rarr)
      }

      def shiftLeft(k: Int, a: Array[T]): Unit = {
        if (k != 0) {
          val d = k >> POW
          if (d != 0) {
            a.indices.foreach { i => if (i + d < a.length) a(i) = a(i + d) }
            (a.length - d until a.length).foreach(a(_) = 0)
          }
          val sh = k & MOD
          if (sh != 0) {
            a.indices.foreach { i =>
              a(i) <<= sh
              if (i + 1 < a.length) {
                a(i) |= a(i + 1) >>> SIZE - sh
              }
            }
          }
        }
      }

      def shiftRight(k: Int): Unit = {
        shiftRight(k, arr)
        shiftLeft(k, rarr)
      }

      def shiftRight(k: Int, a: Array[T]): Unit = {
        if (k != 0) {
          val d = k >> POW
          if (d != 0) {
            a.indices.reverse.foreach { i => // TODO: Reverse
              if (i - d >= 0) a(i) = a(i - d) else a(i) = 0
            }
          }
          val sh = k & MOD
          if (sh != 0) {
            a.indices.reverse.foreach { i => // TODO: Reverse
              a(i) >>>= sh
              if (i - 1 >= 0) {
                a(i) |= a(i - 1) << SIZE - sh
              }
            }
          }
        }
      }

      def copyRaw(l: Int, r: Int): BitArray = {
        val ba = new BitArray(((r >> POW) - (l >> POW) + 1) << POW)
        (l >> POW to r >> POW).foreach(i => ba.arr(i - (l >> POW)) = arr(i)) // TODO: Combine
        (revI(r) >> POW to revI(l) >> POW)
          .foreach(i => ba.rarr(ba.revN((revI(l) >> POW) - i)) = rarr(i)) // TODO: Combine
        ba.const(0, (l & MOD) - 1, zero = true)
        ba.const((l & MOD) + r - l + 1, (((r >> POW) - (l >> POW) + 1) << POW) - 1, zero = true)
        ba
      }

      def copy(l: Int, r: Int): BitArray = {
        val ba: BitArray = copyRaw(l, r)
        ba.shiftLeft(l & MOD)
        ba
      }

      def swap(l1: Int, r1: Int, l2: Int, r2: Int): Unit = { // TODO: Use buffer of arrays to reduce gc
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
          rarr(revN(i)) = 0
          if (ba1.n > 0 &&
            i <= ((ba1Len - 1) >> POW) && i1 < ba1.arr.length) {
            arr(i) |= ba1.arr(i1)
            rarr(revN(i)) |= ba1.rarr(ba1.revN(i1))
            i1 += 1
          }
          if (ba4.n > 0 &&
            ba1Len >> POW <= i && i <= ((ba1Len + ba4Len - 1) >> POW) && i4 < ba4.arr.length) {
            arr(i) |= ba4.arr(i4)
            rarr(revN(i)) |= ba4.rarr(ba4.revN(i4))
            i4 += 1
          }
          if (ba3.n > 0 &&
            (ba1Len + ba4Len) >> POW <= i && i <= ((ba1Len + ba4Len + ba3Len - 1) >> POW) && i3 < ba3.arr.length) {
            arr(i) |= ba3.arr(i3)
            rarr(revN(i)) |= ba3.rarr(ba3.revN(i3))
            i3 += 1
          }
          if (ba2.n > 0 &&
            (ba1Len + ba4Len + ba3Len) >> POW <= i &&
            i <= ((ba1Len + ba4Len + ba3Len + ba2Len - 1) >> POW) &&
            i2 < ba2.arr.length) {
            arr(i) |= ba2.arr(i2)
            rarr(revN(i)) |= ba2.rarr(ba2.revN(i2))
            i2 += 1
          }
          if (ba5.n > 0 &&
            (ba1Len + ba4Len + ba3Len + ba2Len) >> POW <= i && i5 < ba5.arr.length) {
            arr(i) |= ba5.arr(i5)
            rarr(revN(i)) |= ba5.rarr(ba5.revN(i5))
            i5 += 1
          }
        }
      }

      def reverse(l: Int, r: Int): Unit = {
        val ba = copyRaw(l, r)

        const(l, r, zero = true)

        if ((revI(r) & MOD) > (l & MOD)) {
          shiftRight((revI(r) & MOD) - (l & MOD), ba.arr)
          shiftLeft((revI(r) & MOD) - (l & MOD), ba.rarr)
        }
        if ((revI(r) & MOD) < (l & MOD)) {
          shiftLeft((l & MOD) - (revI(r) & MOD), ba.arr)
          shiftRight((l & MOD) - (revI(r) & MOD), ba.rarr)
        }

        ((l >> POW) to (r >> POW)).foreach { i =>
          arr(i) |= ba.rarr(i - (l >> POW) + 1)
        }

        ((revI(r) >> POW) to (revI(l) >> POW)).foreach { i =>
          rarr(i) |= ba.arr(i - (revI(r) >> POW))
        }
      }

      def toString(l: Int, r: Int): String = {
        var skip = SIZE - (revI(l) & MOD) - 1
        val sb = new StringBuilder()
        for (i <- (revI(l) >> POW) to (revI(r) >> POW) by -1) {
          for (j <- 0 until SIZE if sb.length < r - l + 1) {
            if (skip > 0) {
              skip -= 1
            } else {
              sb.append(if (((rarr(i) >>> j) & 1) == 0) 'a' else 'b')
            }
          }
        }
        sb.toString()
      }

      override def toString: String = {
        val sb = new StringBuilder()
        for (i <- last to 0 by -1) {
          for (j <- 0 until SIZE if sb.length < n) {
            sb.append(if (((rarr(i) >>> j) & 1) == 0) 'a' else 'b')
          }
        }
        sb.toString()
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

      def fill(s: String, n: Int, a: Array[T]): Unit = {
        if (s.length != SIZE) sys.error(s"The string length should be eq to $SIZE: ${s.length}")
        s.indices.foreach { i =>
          a(n) <<= 1
          if (s(i) != 'a') a(n) |= 1
        }
      }
    }

    object BitArray
    {
      val POW  = 6
      val SIZE = 64

      def fill(s: String): BitArray = {
        val ba = new BitArray(s.length)
        val last = (s.length - 1) >> POW
        s.grouped(SIZE).zipWithIndex.foreach { case (is, i) =>
          val sb = new StringBuilder(is)
          (is.length until SIZE).foreach { _ => sb.append('a') }
          ba.fill(sb.toString(), i, ba.arr)
          ba.fill(sb.toString().reverse, ba.revN(i), ba.rarr)
        }
        ba
      }
    }

  }

}

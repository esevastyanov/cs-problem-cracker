package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/rust-murderer/problem
  */
object Rust_Murderer extends App
{
  Solution.main(args)

  object Solution
  {

    import scala.collection.mutable

    def rustMurdered(n: Int, s: Int, g: Array[mutable.BitSet]): Array[Int] = {
      val r = Array.fill(n)(1)
      r(s) = 0
      var currDst = 2
      val notVisited = g(s)
      val newlyVisited = mutable.Set[Int]()
      while (notVisited.nonEmpty) {
        notVisited.foreach { i =>
          val diff = notVisited ++ g(i)
          if (diff.size < n) {
            r(i) = currDst
            newlyVisited += i
          }
        }
        notVisited --= newlyVisited
        newlyVisited.clear()
        currDst += 1
      }
      r
    }

    def main(args: Array[String]): Unit = {
      val sc = new FastReader
      (1 to sc.nextInt()).foreach { _ =>
        val n = sc.nextInt()
        val g = Array.fill(n)(mutable.BitSet())
        (1 to sc.nextInt()).foreach { _ =>
          val x = sc.nextInt() - 1
          val y = sc.nextInt() - 1
          g(x) += y
          g(y) += x
        }
        rustMurdered(n, sc.nextInt() - 1, g).foreach(i => if (i != 0) print(i + " "))
        println()
      }
    }

    class FastReader()
    {

      import java.io.DataInputStream

      private val BUFFER_SIZE   = 1 << 16
      private val din           = new DataInputStream(System.in)
      private val buffer        = new Array[Byte](BUFFER_SIZE)
      private var bufferPointer = 0
      private var bytesRead     = 0


      def nextInt(): Int = {
        var ret = 0
        var c = read
        while (c <= ' ') {
          c = read
        }
        val neg = c == '-'
        if (neg) c = read
        do {
          ret = ret * 10 + c - '0'
          c = read
        } while (c >= '0' && c <= '9')
        val res = if (neg) -ret else ret
        res
      }

      def next(): String = {
        val sb = new StringBuilder()
        var c = read
        while (c != -1 && (c == '\n' || c == ' ' || c == '\t')) {
          c = read
        }
        while (c != -1 && c != '\n' && c != ' ' && c != '\t') {
          sb.append(c.toChar)
          c = read
        }
        val res = sb.mkString
        res
      }

      private def fillBuffer(): Unit = {
        bufferPointer = 0
        bytesRead = din.read(buffer, bufferPointer, BUFFER_SIZE)
        if (bytesRead == -1) buffer(0) = -1
      }

      private def read = {
        if (bufferPointer == bytesRead) fillBuffer()
        bufferPointer += 1
        buffer(bufferPointer - 1)
      }

      def close(): Unit = {
        if (din == null) return
        din.close()
      }
    }

  }

}

package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/determining-dna-health/problem
  */
object Determining_DNA_Health extends App
{
  Solution.main(args)

  object Solution
  {

    def main(args: Array[String]): Unit = {
      val sc = new FastReader
      val n = sc.nextInt()
      if (n > 80000) HighGenesNumberSolution.solve(n, sc)
      else LowGenesNumberSolution.solve(n, sc)
    }

    object LowGenesNumberSolution {
      def solve(n: Int, sc: FastReader): Unit = {
        val genes = Array.fill(n)(sc.next())
        val healths = Array.fill(n)(sc.nextInt())

        for (i <- 0 until n) {
          Bohr.addString(genes(i), i)
        }

        var min = Long.MaxValue
        var max = Long.MinValue

        val s = sc.nextInt()
        for (_ <- 1 to s) {
          val first, last = sc.nextInt()
          val d = sc.next()
          var score = Bohr.findAllPos(d, first, last, healths)
          min = math.min(min, score)
          max = math.max(max, score)
        }

        println(s"$min $max")
      }

      object Bohr
      {
        val emptyChar   : Char = (-1).toChar
        var version: Int = 0

        class Vertex(
          val c: Char = emptyChar,
          val parent: Vertex = null,
          var patterns: List[Int] = Nil,
          val nextVertex: Array[Vertex] = new Array[Vertex]('z' - 'a' + 1),
          var autoMove: Array[Vertex] = null,
          var suffFLink: Vertex = null,
          var suffLink: Vertex = null,
          var scores: Long = 0L,
          var version: Int = 0
        )

        val root = new Vertex()

        def addString(s: String, patternNumber: Int): Unit = {
          var v = root
          for (i <- 0 until s.length) {
            val c = s(i)
            val ci = c - 'a'
            v = Option(v.nextVertex(ci)).getOrElse {
              val _v = new Vertex(c, parent = v)
              v.nextVertex(ci) = _v
              _v
            }
          }
          v.patterns = v.patterns :+ patternNumber
        }

        def getSuffLink(v: Vertex): Vertex = {
          if (v.suffLink == null) {
            if (v == root || v.parent == root) {
              v.suffLink = root
            } else {
              v.suffLink = getAutoMove(getSuffLink(v.parent), v.c)
            }
          }
          v.suffLink
        }

        def getAutoMove(v: Vertex, c: Char): Vertex = {
          val i = c - 'a'
          if (v.autoMove == null) v.autoMove = new Array[Vertex]('z' - 'a' + 1)
          if (v.autoMove(i) == null) {
            if (v.nextVertex(i) != null) {
              v.autoMove(i) = v.nextVertex(i)
            } else if (v == root) {
              v.autoMove(i) = root
            } else {
              v.autoMove(i) = getAutoMove(getSuffLink(v), c)
            }
          }
          v.autoMove(i)
        }

        def getSuffFLink(v: Vertex): Vertex = {
          if (v.suffFLink == null) {
            val u = getSuffLink(v)
            if (u == root) {
              v.suffFLink = root
            } else {
              v.suffFLink = if (u.patterns != Nil) u else getSuffFLink(u)
            }
          }
          v.suffFLink
        }

        def findAllPos(s: String, first: Int, last: Int, scores: Array[Int]): Long = {
          version += 1
          var sum = 0L
          def check(v: Vertex): Unit = {
            var u = v
            while (u != root) {
              if (u.patterns.nonEmpty) {
                if (u.version == version) {
                  sum += u.scores
                } else {
                  var _sum = 0L
                  u.patterns.foreach(indx =>
                    if (first <= indx && indx <= last) _sum += scores(indx)
                  )
                  u.version = version
                  u.scores = _sum
                  sum += _sum
                }
              }
              u = getSuffFLink(u)
            }
          }

          var u = root
          s.indices.foreach { i =>
            val c = s(i)
            u = getAutoMove(u, c)
            check(u)
          }
          sum
        }
      }

    }

    object HighGenesNumberSolution
    {

      case class Gene(gene: String, idx: Int, var health: Long = 0)
      case class SymbolInfo(c: Char, minIdx: Int, maxIdx: Int, lengths: Array[Int], mGeneInfos: Map[String, GeneInfo])
      case class GeneInfo(gene: String, minIdx: Int, maxIdx: Int, genes: Array[Gene])

      def reduceLeftBySum(ss: Array[Gene]): Unit = {
        var sum = 0L
        for (i <- 0 until ss.length) {
          val gene = ss(i)
          sum += gene.health
          gene.health = sum
        }
      }

      def findFirstHealth(start: Int, ss: Array[Gene]): Option[Gene] = {
        if (ss(0).idx >= start) return None
        if (ss(ss.length - 1).idx < start) return None
        var l = 0
        var r = ss.length - 1
        var i = 0
        while (l < r) {
          i = (r + l) / 2
          if (ss(i).idx > start) {
            r = i - 1
          } else if (ss(i).idx < start && ss(i + 1).idx < start) {
            l = i + 1
          } else if (ss(i).idx < start && ss(i + 1).idx >= start) {
            return Some(ss(i))
          } else if (ss(i).idx == start) return Some(ss(i - 1))
        }
        Some(ss(l))
      }

      def findLastHealth(last: Int, ss: Array[Gene]): Option[Gene] = {
        if (ss(0).idx > last) return None
        if (ss(ss.length - 1).idx <= last) return Some(ss(ss.length - 1))
        var l = 0
        var r = ss.length - 1
        var i = 0
        while (l < r) {
          i = (r + l) / 2
          if (ss(i).idx > last) {
            r = i - 1
          } else if (ss(i).idx < last && ss(i + 1).idx <= last) {
            l = i + 1
          } else {
            return Some(ss(i))
          }
        }
        Some(ss(l))
      }

      def solve(n: Int, sc: FastReader): Unit = {
        val genes: Array[Gene] = new Array(n)

        for (i <- 0 until n) genes(i) = Gene(sc.next(), i)
        for (i <- 0 until n) genes(i).health = sc.nextInt()

        val s = sc.nextInt()

        val mSymbolInfo = genes.groupBy(_.gene(0)).map {
          case (c, gs) =>
            val minIdx = gs.head.idx
            val maxIdx = gs.last.idx
            val lengths = gs.map(_.gene.length).distinct.sorted
            c -> SymbolInfo(c, minIdx, maxIdx, lengths, gs.groupBy(_.gene).map { case (gene, ggs) =>
              reduceLeftBySum(ggs)
              val minIdx = ggs.head.idx
              val maxIdx = ggs.last.idx
              gene -> GeneInfo(gene, minIdx, maxIdx, ggs)
            }
            )
        }

        var min = Long.MaxValue
        var max = Long.MinValue
        (1 to s).foreach { ni =>
          val first, last = sc.nextInt()
          val d = sc.next()
          var scores = 0L
          for (idx <- 0 until d.length) {
            val c = d(idx)
            val symbolInfoOpt = mSymbolInfo.get(c)
            val sb = new StringBuilder
            scores += symbolInfoOpt.fold(0L) { si =>
              val (sFirst, sLast) = (math.max(si.minIdx, first), math.min(si.maxIdx, last))
              if (sFirst <= sLast) {
                var cScores = 0L
                val ls = si.lengths
                for (li <- 0 until ls.length) {
                  val l = ls(li)
                  cScores += {
                    val l1 = if (idx + l > d.length) {
                      0L
                    } else {
                      for (j <- idx + sb.length until idx + l) {
                        sb += d(j)
                      }
                      val gene = sb.toString()
                      si.mGeneInfos.get(gene).fold(0L) { gi =>
                        val (gFirst, gLast) = (math.max(sFirst, gi.minIdx), math.min(sLast, gi.maxIdx))
                        if (gFirst <= gLast) {
                          val maxSum: Long = findLastHealth(gLast, gi.genes).fold(0L)(_.health)
                          val minSum: Long = findFirstHealth(gFirst, gi.genes).fold(0L)(_.health)
                          maxSum - minSum
                        } else {
                          0L
                        }
                      }
                    }
                    l1
                  }
                }
                cScores
              } else {
                0L
              }
            }
          }
          min = math.min(min, scores)
          max = math.max(max, scores)
        }
        println(s"$min $max")
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

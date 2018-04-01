package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/determining-dna-health/problem
  */
object Determining_DNA_Health_1 extends App
{
  Solution.main(args)

  // TODO: Complete!
  object Solution
  {

    import scala.collection.mutable

    case class Gene(gene: String, idx: Int, var health: Long = 0)

    case class SymbolInfo(
      c: Char,
      var minIdx: Int,
      var maxIdx: Int,
      lengths: mutable.SortedSet[Int],
      mGeneInfos: mutable.Map[String, GeneInfo]
    )

    case class GeneInfo(gene: String, var minIdx: Int, var maxIdx: Int, genes: mutable.ArrayBuffer[Gene])

    def findFirstHealth(start: Int, ss: mutable.ArrayBuffer[Gene]): Option[Gene] = {
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

    def findLastHealth(last: Int, ss: mutable.ArrayBuffer[Gene]): Option[Gene] = {
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

    def main(args: Array[String]) {
      val sc = new java.util.Scanner(System.in)
      val n = sc.nextInt()
      val mGenesCounter = mutable.Map[String, Int]().withDefaultValue(0)
      val genes: Array[Gene] = new Array(n)

      for (i <- 0 until n) {
        val gene = sc.next()
        genes(i) = Gene(gene, i)
        mGenesCounter(gene) += 1
      }
      for (i <- 0 until n) {
        genes(i).health = sc.nextInt()
      }

      val mSymbolInfo: mutable.Map[Char, SymbolInfo] = mutable.Map.empty
      for (i <- 0 until n) {
        val g = genes(i)
        val si = mSymbolInfo.getOrElseUpdate(
          g.gene(0),
          SymbolInfo(g.gene(0), i, i, mutable.SortedSet(), mutable.Map[String, GeneInfo]())
        )
        si.lengths.add(g.gene.length)
        si.minIdx = math.min(si.minIdx, i)
        si.maxIdx = math.max(si.maxIdx, i)
        val gi = si.mGeneInfos.getOrElseUpdate(g.gene,
          GeneInfo(g.gene, i, i, new mutable.ArrayBuffer[Gene](mGenesCounter(g.gene)))
        )
        gi.minIdx = math.min(gi.minIdx, i)
        gi.maxIdx = math.max(gi.maxIdx, i)
        val previousHealth = gi.genes.lastOption.fold(0L)(_.health)
        g.health += previousHealth
        gi.genes.append(g)
      }

      var min = Long.MaxValue
      var max = Long.MinValue
      val s = sc.nextInt()
      (1 to s).foreach { ni =>
        val first, last = sc.nextInt()
        val d = sc.next()
        var scores = 0L
        for (idx <- 0 until d.length) {
          val c = d(idx)
          val symbolInfoOpt = mSymbolInfo.get(c)
          val sb = new StringBuilder
          scores += symbolInfoOpt.fold(0L) { si =>
            val sFirst = math.max(si.minIdx, first)
            val sLast = math.min(si.maxIdx, last)
            if (sFirst <= sLast) {
              var cScores = 0L
              val ls = si.lengths
              ls.foreach { l =>
                cScores += {
                  if (idx + l > d.length) {
                    0L
                  } else {
                    for (j <- idx + sb.length until idx + l) {
                      sb += d(j)
                    }
                    val gene = sb.toString()
                    si.mGeneInfos.get(gene).fold(0L) { gi =>
                      val gFirst = math.max(sFirst, gi.minIdx)
                      val gLast = math.min(sLast, gi.maxIdx)
                      if (gFirst <= gLast) {
                        val maxSum: Long = findLastHealth(gLast, gi.genes).fold(0L)(_.health)
                        val minSum: Long = findFirstHealth(gFirst, gi.genes).fold(0L)(_.health)
                        maxSum - minSum
                      } else {
                        0L
                      }
                    }
                  }
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

  def time[T](s: String)(f: => T): T = {
    val start = System.currentTimeMillis()
    val r = f
    val end = System.currentTimeMillis()
    println(s + ":" + (end - start) + " ms")
    r
  }
}

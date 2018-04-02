package training.hackerrank

/**
  * https://www.hackerrank.com/challenges/determining-dna-health/problem
  */
object Determining_DNA_Health_2 extends App
{
  Solution.main(args)

  // TODO: Complete!
  object Solution
  {

    import scala.collection.mutable
    import scala.collection.mutable.ArrayBuffer

    def main(args: Array[String]): Unit = {
      val sc = new FastReader
      val n = sc.nextInt()
      val genes = time("fill genes")(ArrayBuffer.fill(n)(sc.next()))
      val healths = time("fill healthes")(ArrayBuffer.fill(n)(sc.nextInt()))

      val ac = time("automaton...") {
        val atmtn = new Automaton
        for (i <- 0 until n) {
          val gene = genes(i)
          val health = healths(i)
          atmtn.addWord(gene, Gene(gene, i, health))
        }
        time("automaton fail transitions")(atmtn.setFailTransitions())
        atmtn
      }

      var min = Long.MaxValue
      var max = Long.MinValue
      val s = sc.nextInt()
      for (_ <- 1 to s) {
        val first, last = sc.nextInt()
        val d = sc.next()
        val score = ac.search(d, first, last)
        min = math.min(min, score)
        max = math.max(max, score)
      }
      println(s"$min $max")
    }

    val ENV = "PROD"

    def time[T](s: String)(f: => T): T = {
      val start = System.currentTimeMillis()
      val r = f
      val end = System.currentTimeMillis()
      if (ENV != "PROD") {
        println(s + ":" + (end - start) + " ms")
      }
      r
    }

    case class Gene(gene: String, idx: Int, var health: Long = 0L)

    object Gene
    {
      implicit val ordering: Ordering[Gene] = new Ordering[Gene]
      {
        override def compare(x: Gene, y: Gene): Int = x.idx - y.idx
      }
    }

    class Automaton
    {
      val root: Node = new Node(failure = null)
      root.failure = root

      def addWord(word: String, v: Gene): Unit = {
        var currentNode: Node = root
        for (i <- 0 until word.length) {
          val c = word(i)
          currentNode = currentNode.getNext(c).getOrElse {
            val next = new Node(failure = root)
            currentNode.setNext(c, next)
            next
          }
        }
        currentNode.addOutput(v)
      }

      // set failure function
      def setFailTransitions(): Unit = {
        val queue = mutable.Queue[Node]()
        // set failure for node whose depth=1
        root.getNextNodes.foreach(queue += _)
        while (queue.nonEmpty) {
          val rNode = queue.dequeue()
          for (a <- 'a' to 'z') {
            rNode.getNext(a).foreach {s =>
              queue += s
              var fNextNode = rNode.failure
              while (fNextNode.getNext(a).isEmpty && fNextNode != root) {
                fNextNode = fNextNode.failure
              }
              val goto_a: Node =
                if (fNextNode == root && fNextNode.getNext(a).isEmpty) {
                  root
                } else {
                  fNextNode.getNext(a).getOrElse(root)
                }
              s.failure = goto_a
              s.merge(s.failure)
            }
          }
        }
      }

      def search(str: String, first: Int, last: Int): Long = {
        var sum = 0L
        var node = root
        for (i <- 0 until str.length) {
          val c = str(i)
          var next = node.getNext(c)
          while (next.isEmpty && node != root) {
            node = node.failure
            next = node.getNext(c)
          }
          node = if (node == root && next.isEmpty) root else next.get
          sum += node.getSum(first, last)
        }
        sum
      }
    }

    class Node(
      var failure: Node,
      val next: Array[Node] = Array.fill('z'-'a'+1)(null),
      var output: ArrayBuffer[Gene] = ArrayBuffer[Gene](),
      var genes: ArrayBuffer[Gene] = null
    ) {
      def setNext(c: Char, n: Node): Unit = next('z' - c) = n
      def getNext(c:Char): Option[Node] = Option(next('z' - c))
      def getNextNodes: Seq[Node] = next.view.filterNot(_ == null)

      def getOutput: ArrayBuffer[Gene] = output
      def isOutputEmpty: Boolean = output.isEmpty
      def addOutput(v: Gene): Unit = output += v
      def merge(n: Node): Unit = {
        val res = ArrayBuffer[Gene]()
        var i, j = 0
        while (i < output.length || j < n.getOutput.length) {
          if (i < output.length && j < n.getOutput.length) {
            val o_i = output(i)
            val o_j = n.getOutput(j)
            if (Gene.ordering.lt(o_i, o_j)) {
//              if (Gene.ordering.equiv(o_i, o_j)) j += 1
              res += o_i
              i += 1
            } else {
              res += o_j
              j += 1
            }
          } else if (i < output.length) {
            res += output(i)
            i += 1
          } else {
            res += n.getOutput(j)
            j += 1
          }
        }
        output = res
      }

      def getSum(first: Int, last: Int): Long = {
        summarizeScores()
/*
        var sum = 0L
        output.foreach(g =>
          if (first <= g.idx && g.idx <= last) sum += g.health
        )
        sum
*/
        if (genes.isEmpty) 0
        else {
          val gFirst = math.max(genes.head.idx, first)
          val gLast = math.min(genes.last.idx, last)
          if (gFirst <= gLast) {
            val minSum = findFirstHealth(gFirst).fold(0L)(_.health)
            val maxSum = findLastHealth(gLast).fold(0L)(_.health)
            maxSum - minSum
          } else 0
        }
      }

      private def summarizeScores(): Unit = {
        if (genes != null) return
        genes = new ArrayBuffer[Gene](output.length)
        var sum = 0L
        output.indices.foreach { i =>
          val g = output(i)
          sum += g.health
          genes += g.copy(health = sum)
        }
      }

      private def findFirstHealth(start: Int): Option[Gene] = {
        if (genes(0).idx >= start) return None
        if (genes(genes.length - 1).idx < start) return None
        var l = 0
        var r = genes.length - 1
        var i = 0
        while (l < r) {
          i = (r + l) / 2
          if (genes(i).idx > start) {
            r = i - 1
          } else if (genes(i).idx < start && genes(i + 1).idx < start) {
            l = i + 1
          } else if (genes(i).idx < start && genes(i + 1).idx >= start) {
            return Some(genes(i))
          } else if (genes(i).idx == start) return Some(genes(i - 1))
        }
        Some(genes(l))
      }

      private def findLastHealth(last: Int): Option[Gene] = {
        if (genes(0).idx > last) return None
        if (genes(genes.length - 1).idx <= last) return Some(genes(genes.length - 1))
        var l = 0
        var r = genes.length - 1
        var i = 0
        while (l < r) {
          i = (r + l) / 2
          if (genes(i).idx > last) {
            r = i - 1
          } else if (genes(i).idx < last && genes(i + 1).idx <= last) {
            l = i + 1
          } else {
            return Some(genes(i))
          }
        }
        Some(genes(l))
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
// package training.hackerrank

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
        time("automaton adding words") {
          for (i <- 0 until n) {
            val gene = genes(i)
            val health = healths(i)
            atmtn.addWord(gene, Gene(i, health))
          }
        }
        time("automaton fail transitions")(atmtn.setFailTransitions())
        atmtn
      }

      var min = Long.MaxValue
      var max = Long.MinValue
      time("searching") {
        val s = sc.nextInt()
        for (_ <- 1 to s) {
          val first, last = sc.nextInt()
          val d = sc.next()
          val score = ac.search(d, first, last)
          min = math.min(min, score)
          max = math.max(max, score)
        }
      }
      println(s"$min $max")
    }

    val ENV = "DEBUG"

    def time[T](s: String)(f: => T): T = {
      val start = System.currentTimeMillis()
      val r = f
      val end = System.currentTimeMillis()
      if (ENV != "PROD") {
        println(s + ":" + (end - start) + " ms")
      }
      r
    }

    case class Gene(idx: Int, var health: Long = 0L)

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
            val next = new Node(failure = root, char = c)
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
          rNode.getNextNodes.foreach { s =>
            queue += s
            val a = s.char
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
      val char: Char = Char.MinValue,
      val nextNodes: Array[Node] = new Array[Node]('z' - 'a' + 1),
      var output: List[Gene] = List[Gene](),
      var genes: Array[Gene] = null
    )
    {
      def setNext(c: Char, n: Node): Unit = nextNodes('z' - c) = n

      def getNext(c: Char): Option[Node] = Option(nextNodes('z' - c))

      def getNextNodes: Iterable[Node] = nextNodes.view.filterNot(_ == null)

      def getOutput: Seq[Gene] = output

      def addOutput(v: Gene): Unit = output = output :+ v

      def merge(n: Node): Unit = {
        if (output.isEmpty) output = n.output
        else if (n.output.isEmpty) output = output
        else if (output.last.idx < n.output.head.idx) output = output ++ n.output
        else if (n.output.last.idx < output.head.idx) output = n.output ++ output
        else {
          val res = mutable.ListBuffer[Gene]()
          val i = output.iterator.buffered
          val j = n.output.iterator.buffered
          while (i.hasNext || j.hasNext) {
            if (i.hasNext && j.hasNext) {
              if (Gene.ordering.lt(i.head, j.head)) {
                if (Gene.ordering.equiv(i.head, j.head)) j.next()
                res += i.next()
              } else {
                res += j.next()
              }
            } else if (i.nonEmpty) {
              res += i.next()
            } else {
              res += j.next()
            }
          }
          output = res.toList
        }
      }

      def getSum(first: Int, last: Int): Long = {
        summarizeScores()
        if (output.isEmpty) {
          0L
        } else {
          val gFirst = math.max(output.head.idx, first)
          val gLast = math.min(output.last.idx, last)
          if (gFirst <= gLast) {
            val minSum = findFirstHealth(gFirst).fold(0L)(_.health)
            val maxSum = findLastHealth(gLast).fold(0L)(_.health)
            maxSum - minSum
          } else {
            0L
          }
        }
      }

      private def summarizeScores(): Unit = {
        if (genes != null) return
        genes = new Array[Gene](output.size)
        var sum = 0L
        var i = 0
        output.foreach { g =>
          sum += g.health
          g.health = sum
          genes(i) = g
          i += 1
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

/*
      {
        new Iterable[Node] {
          override def iterator: Iterator[Node] = new Iterator[Node] {
            var n: Node = _
            var i = 0
            override def hasNext: Boolean = {
              while (i < nextNodes.length && { n = nextNodes(i); n == null } ) i += 1
              i < nextNodes.length && n != null
            }
            override def next(): Node = {
              if (n != null) {
                i += 1
                val res = n
                n = null
                res
              } else {
                while (i < nextNodes.length && { n = nextNodes(i); n == null } ) i += 1
                val res = n
                n = null
                res
              }
            }
          }
        }
        nextNodes.view.filterNot(_ == null)
      }
*/

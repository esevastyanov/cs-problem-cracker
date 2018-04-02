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
      val mGenes: mutable.Map[String, ArrayBuffer[Gene]] = time("mGenes...") {
        val m = mutable.Map[String, ArrayBuffer[Gene]]()
        genes.indices.foreach { i =>
          val g = genes(i)
          val gs = m.getOrElseUpdate(g, ArrayBuffer())
          gs.append(Gene(g, i, healths(i) + gs.lastOption.fold(0L)(_.health)))
        }
        m
      }
      val ac = time("automaton...") {
        val atmtn = new Automaton
        time("automaton generation")(mGenes.keys.foreach(atmtn.addWord))
        time("automaton fail transitions")(atmtn.setFailTransitions())
        atmtn
      }
      var min = Long.MaxValue
      var max = Long.MinValue
      val s = sc.nextInt()
      for (_ <- 1 to s) {
        val first, last = sc.nextInt()
        val d = time("fill d")(sc.next())
        var score = 0L
        time(s"search ${d(0)}...")(
          ac.search(d).groupBy(identity).foreach { case (w, gss) =>
            val gGenes = mGenes(w)
            val gFirst = math.max(gGenes.head.idx, first)
            val gLast = math.min(gGenes.last.idx, last)
            if (gFirst <= gLast) {
              val minSum = findFirstHealth(gFirst, gGenes).fold(0L)(_.health)
              val maxSum = findLastHealth(gLast, gGenes).fold(0L)(_.health)
              val _s = gss.size * (maxSum - minSum)
              score += _s
              //            println(s"$d $w ${_s}")
            }
          }
        )
        min = math.min(min, score)
        max = math.max(max, score)
        //        println(s"$d $score")
      }
      println(s"$min $max")
    }

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

    case class Gene(gene: String, idx: Int, var health: Long = 0)

    class Automaton
    {

      class Node(
        val next: Array[Node] = Array.fill('z'-'a'+1)(null),
        val output: mutable.Set[String] = mutable.Set.empty,
        var failure: Node = root
      )

      val root: Node = new Node()
      val empty: Node = null

      private def nextState(node: Node, char: Char): Option[Node] = {
        Option(node.next('z' - char))
      }

      def addWord(word: String): Unit = {
        var currentNode: Node = root
        for (i <- 0 until word.length) {
          val c = word(i)
          currentNode = nextState(currentNode, c).getOrElse {
            val next = new Node()
            currentNode.next('z' - c) = next
            next
          }
        }
        currentNode.output += word
      }

      // set failure function
      def setFailTransitions(): Unit = {
        val queue = mutable.Queue[Node]()
        // set failure for node whose depth=1
        root.next.foreach { s =>
          if (s != empty) queue += s
        }
        while (queue.nonEmpty) {
          val rNode = queue.dequeue()
          for (a <- 'a' to 'z') {
            nextState(rNode, a).foreach {s =>
              queue += s
              var fNextNode = rNode.failure
              while (nextState(fNextNode, a).isEmpty && fNextNode != root) {
                fNextNode = fNextNode.failure
              }
              val goto_a: Node =
                if (fNextNode == root && nextState(fNextNode, a).isEmpty) {
                  root
                } else {
                  nextState(fNextNode, a).getOrElse(root)
                }
              s.failure = goto_a
              s.output ++= s.failure.output
            }
          }
        }
      }

      def search(str: String): mutable.ArrayBuffer[String] = {
        val resultBuffer: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
        var node = root
        for (i <- 0 until str.length) {
          val c = str(i)
          var next = nextState(node, c)
          while (next.isEmpty && node != root) {
            node = node.failure
            next = nextState(node, c)
          }
          node = if (node == root && next.isEmpty) root else next.get
          if (node.output.nonEmpty) {
            node.output.foreach(resultBuffer.append(_))
          }
        }
        resultBuffer
      }

      /*
            // search words
            def search(str: String): mutable.ArrayBuffer[(String, T)] = {
              val resultBuffer: mutable.ArrayBuffer[(String, T)] = mutable.ArrayBuffer()
              var node = trie(0)
              for (i <- 0 until str.length) {
                val c = str(i)
                var next = nextState(node, c)
                while (next == -1 && node.state != 0) {
                  node = trie(node.failure)
                  next = nextState(node, c)
                }
                node = if (node.state == 0 && next == -1) trie(0) else trie(next)
                if (node.output.nonEmpty) {
                  node.output.foreach(resultBuffer.append(_))
                }
              }
              resultBuffer
            }
      */
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
        val sb = new mutable.StringBuilder()
        var c = read
        while (c != -1 && (c == '\n' || c == ' ' || c == '\t')) c = read
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
package training.algorithm

/**
  * https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm
  * https://habrahabr.ru/post/198682/
  */
object Aho_Corasick_Algorithm extends App
{

  object Bohr
  {
    val emptyChar   : Char = (-1).toChar
    val emptyPattern: Int  = -1

    class Vertex(
      val c: Char = emptyChar,
      var patternNumber: Int = emptyPattern,
      val nextVertex: Array[Vertex] = new Array[Vertex]('z' - 'a' + 1),
      val autoMove: Array[Vertex] = new Array[Vertex]('z' - 'a' + 1),
      val parent: Vertex = null,
      var suffLink: Vertex = null,
      var suffFLink: Vertex = null
    )

    val root = new Vertex()

    def addString(s: String, patternNumber: Int): Unit = {
      var v = root
      s.foreach { c =>
        val u = Option(v.nextVertex(c - 'a')).getOrElse(new Vertex(c, parent = v))
        v.nextVertex(c - 'a') = u
        v = u
      }
      v.patternNumber = patternNumber
    }

    def isStringIn(s: String): Boolean = {
      var v = root
      s.foreach { c =>
        v = v.nextVertex(c - 'a')
        if (v == null) return false
      }
      v.patternNumber != emptyPattern
    }

    def getSuffLink(v: Vertex): Vertex = {
      if (v.suffLink == null)
        if (v == root || v.parent == root) v.suffLink = root
        else v.suffLink = getAutoMove(getSuffLink(v.parent), v.c)
      v.suffLink
    }

    def getAutoMove(v: Vertex, c: Char): Vertex = {
      val i = c - 'a'
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
          v.suffFLink = if (u.patternNumber != emptyPattern) u else getSuffFLink(u)
        }
      }
      v.suffFLink
    }

    def check(v: Vertex): Unit = {
      var u = v
      while (u != root) {
        if (u.patternNumber != emptyPattern) println(u.patternNumber)
        u = getSuffFLink(u)
      }
    }

    def findAllPos(s: String): Unit = {
      var u = root
      s.foreach { c =>
        u = getAutoMove(u, c)
        check(u)
      }
    }
  }

  val sc = new java.util.Scanner(System.in)
  (1 to sc.nextInt()).foreach { i =>
    val str = sc.next()
    Bohr.addString(str, i)
  }
  val text = sc.next()
  Bohr.findAllPos(text)
}

package training.algorithm

object SuffixArray
{

  import scala.collection.mutable

  val alphabetSize: Int = 'z' - 'a' + 2

  implicit class IndexOpt(c: Char)
  {
    val idx: Int = if (c == '$') 0 else c - 'a' + 1
  }

  implicit class BoolOps(b: Boolean)
  {
    val toInt: Int = if (b) 1 else 0
  }

  def sortCharacters(s: String): Array[Int] = {
    val order = new Array[Int](s.length)
    val count = new Array[Int](alphabetSize)
    for (i <- s.indices) {
      count(s(i).idx) += 1
    }
    for (j <- 1 until alphabetSize) {
      count(j) += count(j - 1)
    }
    for (i <- s.indices.reverse) {
      count(s(i).idx) -= 1
      order(count(s(i).idx)) = i
    }
    order
  }

  def computeCharClasses(s: String, order: Array[Int]): Array[Int] = {
    val clazz = new Array[Int](s.length)
    clazz(order(0)) = 0
    for (i <- 1 until s.length) {
      clazz(order(i)) = clazz(order(i - 1)) + (s(order(i)) != s(order(i - 1))).toInt
    }
    clazz
  }

  def sortDoubled(s: String, l: Int, order: Array[Int], clazz: Array[Int]): Array[Int] = {
    val count = new Array[Int](s.length)
    val newOrder = new Array[Int](s.length)
    for (i <- s.indices) {
      count(clazz(i)) += 1
    }
    for (j <- 1 until s.length) {
      count(j) += count(j - 1)
    }
    for (i <- s.indices.reverse) {
      val start = (order(i) - l + s.length) % s.length
      val cl = clazz(start)
      count(cl) -= 1
      newOrder(count(cl)) = start
    }
    newOrder
  }

  def updateClasses(newOrder: Array[Int], clazz: Array[Int], l: Int): Array[Int] = {
    val n = newOrder.length
    val newClazz = new Array[Int](n)
    newClazz(newOrder(0)) = 0
    for (i <- 1 until n) {
      val cur = newOrder(i)
      val prev = newOrder(i - 1)
      val mid = (cur + l) % n
      val midPrev = (prev + l) % n
      newClazz(cur) = newClazz(prev) + (clazz(cur) != clazz(prev) || clazz(mid) != clazz(midPrev)).toInt
    }
    newClazz
  }

  def buildSuffixArray(s: String): Array[Int] = {
    var order = sortCharacters(s)
    var clazz = computeCharClasses(s, order)
    var l = 1
    while (l < s.length) {
      order = sortDoubled(s, l, order, clazz)
      clazz = updateClasses(order, clazz, l)
      l <<= 1
    }
    order
  }

  def invertSuffixArray(order: Array[Int]): Array[Int] = {
    val pos = new Array[Int](order.length)
    for (i <- order.indices) {
      pos(order(i)) = i
    }
    pos
  }

  def lcpOfSuffixes(s: String, i: Int, j: Int, equal: Int): Int = {
    var lcp = math.max(0, equal)
    while ((i + lcp < s.length) && (j + lcp < s.length) && s(i + lcp) == s(j + lcp)) {
      lcp += 1
    }
    lcp
  }

  def computeLCPArray(s: String, order: Array[Int]): Array[Int] = {
    val lcpArray = new Array[Int](s.length - 1)
    var lcp = 0
    val posInOrder = invertSuffixArray(order)
    var suffix = order(0)
    for (_ <- s.indices) {
      val orderIndex = posInOrder(suffix)
      if (orderIndex == s.length - 1) {
        lcp = 0
      } else {
        val nextSuffix = order(orderIndex + 1)
        lcp = lcpOfSuffixes(s, suffix, nextSuffix, lcp - 1)
        lcpArray(orderIndex) = lcp
      }
      suffix = (suffix + 1) % s.length
    }
    lcpArray
  }

  def stFromSA(s: String, order: Array[Int], lcpArray: Array[Int]): SuffixTreeNode = {
    val root = new SuffixTreeNode()
    var lcpPrev = 0
    var curNode = root
    for (i <- s.indices) {
      val suffix = order(i)
      while (curNode.stringDepth > lcpPrev) {
        curNode = curNode.parent
      }
      if (curNode.stringDepth != lcpPrev) {
        val edgeStart = order(i - 1) + curNode.stringDepth
        val offset = lcpPrev - curNode.stringDepth
        val midNode = SuffixTreeNode.breakEdge(curNode, s, edgeStart, offset)
        curNode = midNode
      }
      curNode = SuffixTreeNode.createNewLeaf(curNode, s, suffix)
      if (i < s.length - 1) {
        lcpPrev = lcpArray(i)
      }
    }
    root
  }

  def buildSuffixTree(s: String): SuffixTreeNode = {
    val sModified = s + "$"
    val order = buildSuffixArray(sModified)
    val lcp = computeLCPArray(sModified, order)
    val tree = stFromSA(sModified, order, lcp)
    tree
  }

  class SuffixTreeNode(
    var parent: SuffixTreeNode = null,
    val children: mutable.Map[Char, SuffixTreeNode] = mutable.Map.empty,
    var stringDepth: Int = 0,
    var edgeStart: Int = -1,
    var edgeEnd: Int = -1
  )

  object SuffixTreeNode
  {
    def createNewLeaf(node: SuffixTreeNode, s: String, suffix: Int): SuffixTreeNode = {
      val leaf =
        new SuffixTreeNode(
          parent = node,
          stringDepth = s.length - suffix,
          edgeStart = suffix + node.stringDepth,
          edgeEnd = s.length - 1
        )
      node.children(s(leaf.edgeStart)) = leaf
      leaf
    }

    def breakEdge(node: SuffixTreeNode, s: String, start: Int, offset: Int): SuffixTreeNode = {
      val startChar = s(start)
      val midChar = s(start + offset)
      val midNode = new SuffixTreeNode(
        parent = node,
        stringDepth = node.stringDepth + offset,
        edgeStart = start,
        edgeEnd = start + offset - 1
      )
      midNode.children(midChar) = node.children(startChar)
      node.children(startChar).parent = midNode
      node.children(startChar).edgeStart += offset
      node.children(startChar) = midNode
      midNode
    }
  }


}

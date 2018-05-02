package training.hacherrank

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.util.Try
import training.hackerrank.Hamming_Distance.Solution.BitArray

class Hamming_Distance_Test extends FunSuite with Matchers
{

  test("const") {
    val lengthLimit = BitArray.SIZE * 3
    for (l <- 1 to lengthLimit) {
      val s = Array.fill(l)(if (math.random() < .5) 'a' else 'b').mkString
      for (i <- 0 until math.min(BitArray.SIZE, l)) {
        for (j <- math.max(i, l - BitArray.SIZE) until l) {

          {
            val ba = BitArray.fill(s)
            ba.const(i, j, 'a')
            val res =
              Try(s.substring(0, i)).getOrElse("") +
                Array.fill(j - i + 1)('a').mkString +
                Try(s.substring(j + 1)).getOrElse("")
            ba.toString shouldBe res
          }

          {
            val ba = BitArray.fill(s)
            ba.const(i, j, 'b')
            val res =
              Try(s.substring(0, i)).getOrElse("") +
                Array.fill(j - i + 1)('b').mkString +
                Try(s.substring(j + 1)).getOrElse("")
            ba.toString shouldBe res
          }
        }
      }
    }
  }

  test("shift left") {
    val lengthLimit = BitArray.SIZE * 3
    for (l <- 1 to lengthLimit) {
      val s = Array.fill(l)(if (math.random() < .5) 'a' else 'b').mkString
      for (i <- 0 until l) {
        val ba = BitArray.fill(s)
        ba.shiftLeft(i)
        val res = s.substring(i) +
          Array.fill(i)('a').mkString
        ba.toString shouldBe res
      }
    }
  }

  test("copy raw") {
    val lengthLimit = BitArray.SIZE * 3
    for (l <- 1 to lengthLimit) {
      val s = Array.fill(l)(if (math.random() < .5) 'a' else 'b').mkString
      for (i <- 0 until math.min(BitArray.SIZE, l)) {
        for (j <- math.max(i, l - BitArray.SIZE) until l) {
          val ba = BitArray.fill(s).copyRaw(i, j)
          var res = Try(Array.fill(i % BitArray.SIZE)('a').mkString).getOrElse("") + s.substring(i, j + 1)
          while (res.length % BitArray.SIZE != 0) {
            res += 'a'
          }
          ba.toString shouldBe res
        }
      }
    }
  }

  test("copy") {
    val lengthLimit = BitArray.SIZE * 3
    for (l <- 1 to lengthLimit) {
      val s = Array.fill(l)(if (math.random() < .5) 'a' else 'b').mkString
      for (i <- 0 until math.min(BitArray.SIZE, l)) {
        for (j <- math.max(i, l - BitArray.SIZE) until l) {
          val ba = BitArray.fill(s).copy(i, j).toString(0, j - i)
          var res = s.substring(i, j + 1)
          ba.toString shouldBe res
        }
      }
    }
  }

  test("hamming") {
    val lengthLimit = BitArray.SIZE * 3
    for (l <- 33 to lengthLimit) {
      val s = Array.fill(l)(if (math.random() < .5) 'a' else 'b').mkString
      val ba = BitArray.fill(s)
      for (i1 <- 0 until l) {
        for (i2 <- 0 until l) {
          for (l1 <- 1 to math.min(l - i1, l - i2)) {
            var res = 0
            for (j <- 0 until l1 if s(i1 + j) != s(i2 + j)) res += 1
            ba.hamming(i1, i2, l1) shouldBe res
          }
        }
      }
    }
  }

}

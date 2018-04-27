package training.hacherrank

import org.scalatest.FunSuite
import org.scalatest.Matchers
import training.hackerrank.Hamming_Distance.Solution.BitArray

class Hamming_Distance_Test extends FunSuite with Matchers
{
  test("1") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbb")
    ba.const(8, 88, false)
    ba.toString shouldBe
      "aaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  }

  test("2") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaaaaaaaaaa")
    ba.const(8, 95, false)
    ba.toString shouldBe
      "aaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  }

  test("3") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbb")
    ba.const(8, 56, false)
    ba.toString shouldBe
      "aaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  }

  test("4") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbb")
    ba.const(0, 63, true)
    ba.toString shouldBe
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  }

  test("5") {
    val ba =
      BitArray.fill("baaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbbaaaaaaaabbbbbbbb")
    ba.const(1, 62, true)
    ba.toString shouldBe
      "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
  }

  test("6") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaa")
    ba.const(8, 15, true)
    ba.toString shouldBe
      "aaaaaaaaaaaaaaaaaaaaaaaa"
  }

  test("7") {
    val ba =
      BitArray.fill("aaaaaaaabbbbbbbbaaaaaaaa")
    ba.const(7, 16, false)
    ba.toString shouldBe
      "aaaaaaabbbbbbbbbbaaaaaaa"
  }

}

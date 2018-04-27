package training.hacherrank

import org.scalatest._
import training.hackerrank.Equal

class EqualTest extends FunSuite with Matchers
{
  test("case 1") {
    Equal.Solution.operations(Seq(2, 2, 3, 7)) shouldBe 2L
  }

  test("case 2") {
    Equal.Solution.operations(Seq(2, 2, 3, 7, 7, 7)) shouldBe 4L
  }

  test("case 3") {
    Equal.Solution.operations(Seq(1, 5, 5)) shouldBe 3L
  }

  test("case 4") {
    Equal.Solution.operations(Seq(
      53,361,188,665,786,898,447,562,272,123,229,629,670,848,994,54,822,46,208,17,449,302,466,832,931,778,156,39,31,777,
      749,436,138,289,453,276,539,901,839,811,24,420,440,46,269,786,101,443,832,661,460,281,964,278,465,247,408,622,638,
      440,751,739,876,889,380,330,517,919,583,356,83,959,129,875,5,750,662,106,193,494,120,653,128,84,283,593,683,44,
      567,321,484,318,412,712,559,792,394,77,711,977,785,146,936,914,22,942,664,36,400,857)
    ) shouldBe 10605L
  }

  test("case 5") {
    Equal.Solution.operations(Seq(2, 5, 5, 5, 5, 5)) shouldBe 6L
  }

  test("case 6") {
    Equal.Solution.operations(Seq(1, 5, 5, 10, 10)) shouldBe 7L
  }

  test("case 7") {
    Equal.Solution.operations(Seq(0, 0, 0, 0)) shouldBe 0L
  }

  test("case 8") {
    Equal.Solution.operations(Seq(1, 0, 0, 0)) shouldBe 1L
  }

  test("case 9") {
    Equal.Solution.operations(Seq(1, 1, 1, 1)) shouldBe 0L
  }
}

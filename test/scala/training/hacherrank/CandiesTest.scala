package training.hacherrank

import org.scalatest._
import training.hackerrank.Candies

class CandiesTest extends FunSuite with ShouldMatchers
{
  test("case 1") {
    Candies.Solution.candies(Range(6, 1, -1)) shouldBe 15L
  }
}

package training

import org.scalatest._

class TwitterTaskTest extends FunSuite with ShouldMatchers
{
  test("case 1") {
    TwitterTask.solution_01(Array(1, 2, 3, 4, 5)) shouldBe 0
    TwitterTask.solution_01(Array(1, 2, 5, 4, 5)) shouldBe 1
    TwitterTask.solution_01(Array(5, 2, 5, 4, 5)) shouldBe 4
    TwitterTask.solution_01(Array(5, 2, 5, 5, 5)) shouldBe 3
    TwitterTask.solution_01(Array(5, 5, 5, 5, 5)) shouldBe 0
    TwitterTask.solution_01(Array(5, 4, 3, 2, 1)) shouldBe 0
    TwitterTask.solution_01(Array(5, 4, 3, 2, 5)) shouldBe 6
  }
}

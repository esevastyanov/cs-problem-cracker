import org.scalatest._

class SimpleClassTest extends FunSuite with ShouldMatchers
{
  test("1"){
    new SimpleClass shouldBe 0
  }
}

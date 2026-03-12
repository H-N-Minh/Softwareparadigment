import scala.collection.mutable._

class TestsBasicParser extends TestsBasic {

  override def expectResult(program: String, expectedResult: ExpValue, input: Queue[ExpValue], expectedOutput: String): Unit = {
    expectValidGrammar(program)
  }
}

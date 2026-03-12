import scala.collection.mutable._

class TestsPublicParser extends TestsPublic {

  override def expectResult(program: String, expectedResult: ExpValue, input: Queue[ExpValue], expectedOutput: String): Unit = {
    expectValidGrammar(program)
  }
}

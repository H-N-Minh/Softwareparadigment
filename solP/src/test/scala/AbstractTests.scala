import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar._

import scala.collection.mutable._

abstract class AbstractTests extends AnyFunSuite with TimeLimitedTests {

  val timeLimit = 500 millis

  def expectValidGrammar(prog: String) = {
    val parseResult = ExpParser.parse(prog)
    if(!parseResult.successful) {
      fail(parseResult.toString)
    }
  }

  def expectInvalidGrammar(prog: String) = {
    assert(!ExpParser.parse(prog).successful)
  }

  def expectResult(program: String, expectedResult: ExpValue): Unit = expectResult(program, expectedResult, new Queue(), "")

  def expectResult(program: String, expectedResult: ExpValue, input: Queue[ExpValue], expectedOutput: String): Unit = {
    assertResult(EvaluationResultSuccess(expectedResult, expectedOutput)) {
      Main.evaluateTest(program, input)
    }
  }
}

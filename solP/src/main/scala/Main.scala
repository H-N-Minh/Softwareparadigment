// -----------------------------------------------------
//              DO NOT change this file!
//       The CLI I/O should remain untouched
//          because we need it for testing!
// -----------------------------------------------------

import scala.collection.mutable._

sealed trait EvaluationResult

case class EvaluationResultSuccess(value: ExpValue, output: String) extends EvaluationResult

case class EvaluationResultError(output: String) extends EvaluationResult

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case "file" :: file :: Nil => evaluateProgram(readFile(file))
      case "evalexpr" :: rest => evaluateProgram(rest.mkString(" "))
      case "parser" :: file :: Nil => {
        val parseResult = ExpParser.parse(readFile(file))
        if (parseResult.successful) {
          Console.println(parseResult.get)
          Console.println("==Parse Success==")
        } else {
          Console.println(parseResult.toString)
          Console.println("==Parse Error==")
        }
      }
      case "test" :: file :: Nil => evaluateTest(readFile(file), new Queue()) match {
        case EvaluationResultSuccess(value, output) => Console.println("==Result: " + Utils.format(value) + " Output: " + output + "==")
        case EvaluationResultError(_) => Console.println("==Parse Error==")
      }

      case _ => {
        Console.println("Usage:")
        Console.println("  file [filename] - Loads and interprets the given file.")
        Console.println("  evalexpr [program] - Evaluates the program code given as argument.")
        Console.println("  parser [filename] - Loads and parses the given file.")
        Console.println("  test [filename] - Loads and interprets the given file while buffering all output.")
      }
    }
  }

  private def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)
    val fileString = source.mkString
    source.close()
    fileString
  }

  private def evaluateProgram(program: String): Unit = {
    val parseResult = ExpParser.parse(program)
    if (parseResult.successful) {
      val result = new Interpreter(() => ExpString(scala.io.StdIn.readLine()), Console.print).interpret(parseResult.get)
      Console.println("\n\nResult: " + Utils.format(result))
    } else {
      Console.println("\n\nError: " + parseResult.toString)
    }
  }

  def evaluateTest(program: String, input: Queue[ExpValue]): EvaluationResult = {
    val parseResult = ExpParser.parse(program)
    val output: StringBuilder = new StringBuilder();
    if (parseResult.successful) {
      EvaluationResultSuccess(new Interpreter(() => input.dequeue(), (str: String) => output ++= str).interpret(parseResult.get), output.toString)
    } else {
      EvaluationResultError(parseResult.toString)
    }
  }
}

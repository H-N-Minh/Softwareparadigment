// These classes represent the data types used by the EXP interpreter.
import scala.collection.mutable
import Utils._

// DO NOT change these!
sealed trait ExpValue

case class ExpInteger(i: Int) extends ExpValue

case class ExpList(list: List[ExpValue]) extends ExpValue

case class ExpBoolean(b: Boolean) extends ExpValue

case class ExpString(str: String) extends ExpValue

// You can modify these as you wish.
case class ExpFunction(e: Environment, fun: NodeFunction) extends ExpValue

case class ExpBuiltin(fun: List[ExpValue] => ExpValue) extends ExpValue

// TODO: Add new data types if necessary
case class ExpAnonFunction(params: List[String], body: Node, env: Environment) extends ExpValue

case class ExpOperators(fields: mutable.Map[String, ExpValue]) extends ExpValue

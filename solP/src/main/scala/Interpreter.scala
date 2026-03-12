import scala.language.{existentials, postfixOps}
import Utils._
import scala.collection.mutable
import scala.collection.immutable


class Interpreter(reader: () => ExpValue, writer: String => _) {

  private val builtinFunctions: Map[String, List[ExpValue] => ExpValue] =
    Map(
      "eq?" -> {
        case List(a, b) => ExpBoolean(a == b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "add" -> {
        case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a + b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "sub" -> {
        case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a - b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "mult" -> {
        case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a * b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "lt?" -> {
        case List(ExpInteger(a), ExpInteger(b)) => ExpBoolean(a < b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "first" -> {
        case List(ExpList(Nil)) => ExpList(Nil)
        case List(ExpList(h :: _)) => h
        case List(_) => ExpList(Nil)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "rest" -> {
        case List(ExpList(Nil)) => ExpList(Nil)
        case List(ExpList(_ :: t)) => ExpList(t)
        case List(_) => ExpList(Nil)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "build" -> {
        case List(x, ExpList(l)) => ExpList(x :: l)
        case Nil => ExpList(Nil)
        case List(_, y) => y
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "atom?" -> {
        case List(x) => ExpBoolean(x match { case ExpList(_) => false case _ => true })
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "empty?" -> {
        case List(x) => ExpBoolean(x == ExpList(List()))
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "and" -> {
        case List(ExpBoolean(a), ExpBoolean(b)) => ExpBoolean(a && b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "or" -> {
        case List(ExpBoolean(a), ExpBoolean(b)) => ExpBoolean(a || b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "not" -> {
        case List(ExpBoolean(b)) => ExpBoolean(!b)
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "print" -> {
        case List(str) => writer(Utils.format(str)); str
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      },
      "read" -> {
        case Nil => reader()
        case _ => throw new IllegalArgumentException("Illegal argument(s) for built-in function.")
      }
    )
  // variable relevant to task 5
  private var operatorEnv: Map[String, List[String]] = Map.empty

  def interpret(program: Program): ExpValue = interpret(newEnvironment(), constructFunctionEnvironment(program.funDecls), program.main)

  def interpret(e: Environment, fe: FunctionEnvironment, n: Node): ExpValue = n match {
    case NodeValue(value) => value
    case NodeVariable(name) => getValue(e, fe, name)
    case NodeCond(cond, positive, negative) => interpret(e, fe, if (interpret(e, fe, cond) match {
      case ExpBoolean(b) => b
      case _ => throw new IllegalArgumentException("Illegal type for condition.")
    }) positive else negative)
    case NodeCall(name, args) => interpret(e, fe, name) match {
      case ExpBuiltin(fun) => fun(args map (arg => interpret(e, fe, arg)))
      case ExpFunction(locale: Environment, NodeFunction(params, body)) =>
        val argsValues = args map (arg => interpret(e, fe, arg))
        val paramMap = mutable.Map(params zip argsValues: _*)
        val functionEnv = paramMap :: locale
        interpret(functionEnv, fe, body)
      case ExpAnonFunction(params, body, capturedEnv) =>
        val argValues = args.map(a => interpret(e, fe, a))
        val paramMap = mutable.Map(params zip argValues: _*)
        val functionEnv: Environment = paramMap :: capturedEnv.map(_.clone())
        interpret(functionEnv, fe, body);

      case v => throw new IllegalArgumentException(s"Expected a function, but got $v")
    }


    case NodePlus(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpInteger(x), ExpInteger(y)) => ExpInteger(x + y)
      case (ExpList(x), ExpList(y)) => ExpList(x ++ y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for + operator")
    }
    case NodeMinus(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpInteger(x), ExpInteger(y)) => ExpInteger(x - y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for - operator")
    }


    case NodeMult(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpInteger(x), ExpInteger(y)) => ExpInteger(x * y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for * operator")
    }
    case NodeDiv(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpInteger(x), ExpInteger(y)) => ExpInteger(x / y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for / operator")
    }

    case NodeEq(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpBoolean(x), ExpBoolean(y)) => ExpBoolean(x == y)
      case (ExpInteger(x), ExpInteger(y)) => ExpBoolean(x == y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for == operator")
    }
    case NodeNe(lhs, rhs) => (interpret(e, fe, lhs), interpret(e, fe, rhs)) match {
      case (ExpBoolean(x), ExpBoolean(y)) => ExpBoolean(x != y)
      case (ExpInteger(x), ExpInteger(y)) => ExpBoolean(x != y)
      case (_, _) => throw new IllegalArgumentException("Illegal type(s) for != operator")
    }

    case NodeFunction(params, body) => ExpFunction(e, NodeFunction(params, body))

    // task 1: List literals
    case NodeList(nodes) => ExpList(nodes.map(node_ => interpret(e, fe, node_)))

    // task 3: Sequence-Expressions
    case NodeSeq(stmts, result) => {
      val localMap = mutable.Map[String, ExpValue]()
      val localEnv = localMap :: e

      // interpret each statement in the sequence
      stmts.foreach(stmt => interpretStmt(localEnv, fe, stmt))

      interpret(localEnv, fe, result)
    }

    // task 4: Anonymus Functions

    case NodeFuncLit(params, body) => {
      val captured: Environment = e.map(m => mutable.Map.from(m))
      ExpAnonFunction(params, body, captured)
    }

    // task 5: Dot Operators
    case NodeRecordDecl(className, attributesName) => {
      operatorEnv += (className -> attributesName)
      ExpList(Nil)
    }
    case NodeRecordInst(className, attributesValues) => {
      val fields = operatorEnv.getOrElse(className, throw new IllegalArgumentException(s"$className class name not defined"))
      require(fields.length == attributesValues.length, "Length missmatch")
      val argValues = attributesValues.map(v => interpret(e, fe, v))
      ExpOperators(mutable.Map(fields zip argValues: _*))
    }
    case NodeFieldAccess(instance, attributesName) => {
      interpret(e, fe, instance) match {
        case ExpOperators(fieldsMap)
        => fieldsMap.getOrElse(attributesName, throw new IllegalArgumentException("Attribute not found"))
        case other => throw new IllegalArgumentException("not a record")
      }
    }
    case NodeFieldAssign(instance, attributesName, attributesValues) => {
      interpret(e, fe, instance) match {
        case ExpOperators(fieldsMap) =>
          val evalValue = interpret(e, fe, attributesValues)

          if (!fieldsMap.contains(attributesName))
            throw new IllegalArgumentException("Field not found")
          fieldsMap(attributesName) = evalValue
          ExpList(Nil)

        case other =>
          throw new IllegalArgumentException("Can not assign a field")
      }
    }
    // NOTE: Case that catch everything else, so this should always be the last case
    case n => throw new NotImplementedError(s"Interpretation of node $n not implemented.")

  }


  // helper func for NodeSeq(stmts, result). Hanlde declaration and assignment statements in the sequence (task 3 related)
  private def interpretStmt(e: Environment, fe: FunctionEnvironment, stmt: Node): Unit = stmt match {
    case NodeVarDecl(name, value) => {
      val evaluatedValue = interpret(e, fe, value)
      e.head += (name -> evaluatedValue) // e.head returns the most-local environment (the last one added)
    }
    case NodeVarAssign(name, value) => {
      val evaluatedValue = interpret(e, fe, value)
      e.find(_.contains(name)) match { // e.find goes through each env in the list until it finds one that contains the variable
        case Some(map) => map(name) = evaluatedValue
        case None => throw new IllegalArgumentException(s"Variable $name not declared yet bro.")
      }
    }

    case NodeRecordDecl(name, field) => {
      operatorEnv += (name -> field)
    }



    // task 6: Loops
    case NodeWhile(condition, body) => {
      // first evaluate the condition, if its true then evaluate each statement in the body
      while (interpret(e, fe, condition) match {
        case ExpBoolean(true) => true
        case ExpBoolean(false) => false
      }) {
        body.foreach(stmt => interpretStmt(e, fe, stmt))
      }
      ExpList(Nil)
    }
    case _ => throw new IllegalArgumentException(s"Unsupported statement in sequence: $stmt")
  }

  // modified to work with the new Environment type: search through the list of env first, and then search in the function environment (task 3 related)
  private def getValue(e: Environment, fe: FunctionEnvironment, name: String): ExpValue = {
    e.collectFirst { case map if map.contains(name) => map(name) }.getOrElse(
      fe.getOrElse(name, throw new IllegalArgumentException(s"ID $name not found."))
    )
  }

  // modified to create new env type (task 3 related)
  private def newEnvironment(): Environment = List(mutable.Map[String, ExpValue]())


  private def constructFunctionEnvironment(funDecls: List[Node]): FunctionEnvironment = {
    funDecls.foldLeft(interpretBuiltinFcts()) { (fe, decl) =>
      decl match {
        case NodeFunctionDecl(name, fun) => fe + (name -> interpret(newEnvironment(), fe, fun))
        case NodeRecordDecl(rName, fields) => {
          operatorEnv += (rName -> fields)
          fe
        }
          case _ => fe
      }
    }
  }

  private def interpretBuiltinFcts(): FunctionEnvironment =
    builtinFunctions.map { case (name, fun) => name -> ExpBuiltin(fun) }
}
  // TODO: Add the semantic rules for all missing syntax elements


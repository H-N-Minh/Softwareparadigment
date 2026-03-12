import scala.util.parsing.combinator._

object ExpParser extends RegexParsers {

  def parse(code: String): ParseResult[Program] = parseAll(program, code)

  // def program: Parser[Program] = rep(globFun <~ ";") ~ expr ^^ {
  //   case ls ~ expr => Program(ls, expr)
  // }

  def program: Parser[Program] = rep((globFun | recordDecl) <~ ";") ~ expr ^^ {
    case decls ~ expr => Program(decls.collect { case f: NodeFunctionDecl => f }, expr)
  }

  private def globFun: Parser[NodeFunctionDecl] = ("$" ~> id) ~ ("(" ~> repsep(id, ",") <~ ")") ~ ("=>" ~> expr) ^^ {
    case name ~ params ~ body => NodeFunctionDecl(name, NodeFunction(params, body))
  }


  // private def expr0: Parser[Node] = ((constant | list | cond | variable | fun |seq | paren) ~ rep(call)) ^^ {
  //   case e ~ Nil => e
  //   case e ~ l => l.foldLeft(e)((e, args) => NodeCall(e, args))
  // }

  private def expr0: Parser[Node] = ((constant | list | cond | idBasedExpr | fun | seq | paren) ~ rep(call | fieldAccess)) ^^ {
    case e ~ Nil => e
    case e ~ suffixes => suffixes.foldLeft(e) {
      case (e, args: List[Node]) => NodeCall(e, args)
      case (e, field: String) => NodeFieldAccess(e, field)
      case _ => throw new MatchError("Unexpected suffix type")
    }
  }

  private def int: Parser[Node] = """(0)|([1-9][0-9]*)|(-[1-9][0-9]*)""".r ^^ (value => NodeValue(ExpInteger(value.toInt)))

  private def constant: Parser[Node] = (int | "True" | "False" | "Nil") ^^ {
    case n: Node => n
    case "True" => NodeValue(ExpBoolean(true))
    case "False" => NodeValue(ExpBoolean(false))
    case "Nil" => NodeValue(ExpList(Nil))
  }

  private def cond: Parser[Node] = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
    case cond ~ positive ~ negative => NodeCond(cond, positive, negative)
  }

  private def variable: Parser[Node] = id ^^ (id => NodeVariable(id))

  private def id: Parser[String] = """[a-z][A-Za-z0-9?_]*""".r

  private def paren: Parser[Node] = "(" ~> expr <~ ")"

  private def call: Parser[List[Node]] = "(" ~> repsep(expr, ",") <~ ")"
  // TODO add all missing grammar rules

  // task 1: List literals
  private def list: Parser[Node] = "[" ~> repsep(expr, ",") <~ "]" map { elems =>  NodeList(elems) }

  // task 2: Multiplication and Comparison Operators
  private def expr: Parser[Node] = eqOrNot

  private def eqOrNot: Parser[Node] = chainl1(plsOrMinus, "==" ^^^  ((a: Node, b: Node) => NodeEq(a, b)) |
    "!=" ^^^  ((a: Node, b: Node) => NodeNe(a, b)))

  private def plsOrMinus: Parser[Node] = chainl1(mulOrDev, "+" ^^^ ((a: Node, b: Node) => NodePlus(a, b)) |
    "-" ^^^ ((a: Node, b: Node) => NodeMinus(a, b)))


  private def mulOrDev: Parser[Node] = chainl1(expr0, "*" ^^^ ((a: Node, b: Node) => NodeMult(a, b)) |
    "/" ^^^ ((a: Node, b:Node) => NodeDiv(a, b)))

  // task 3: Sequence-Expressions
  private def seq: Parser[Node] = "{" ~> rep(stmt <~ ";" ) ~ expr <~ "}" ^^ {
    case ss ~ last => NodeSeq(ss, last)
  }
  private def stmt: Parser[Node] = varDecl | varAssign | loop

  private def stmt2: Parser[Node] =  varAssign | loop

  private def varDecl: Parser[Node] = ("$" ~> id)~ (":=" ~> expr) ^^{
    case name ~ value => NodeVarDecl(name, value)
  }

  private def varAssign: Parser[Node] = (id ~ rep(recordAccess) <~ ":=") ~ expr ^^ {
    case baseId ~ fieldNames ~ value =>
      if (fieldNames.isEmpty)
      {
        NodeVarAssign(baseId, value)
      }
      else
      {
        val baseNode: Node = NodeVariable(baseId)
        val targetInstance: Node = fieldNames.init.foldLeft(baseNode)
        {
          (currentBase, fieldName) => NodeFieldAccess(currentBase, fieldName)
        }
        val finalFieldName: String = fieldNames.last
        NodeFieldAssign(targetInstance, finalFieldName, value)
      }
  }
  
  // task 4: Anonymous Functions
  private def fun: Parser[Node] =  ("(" ~> repsep(id, ",") <~ ")") ~ ("=>" ~> expr) ^^ {
    case params ~ body => NodeFuncLit(params, body)
  }

  // task 5: Records and Dot operator
  private def recordDecl: Parser[NodeRecordDecl] = ("$" ~> id) ~ ("{" ~> repsep(id, ",") <~ "}") ^^ {
    case className ~ attributesName => NodeRecordDecl(className, attributesName)
  }

  private def recordInst: Parser[Node] = id ~ ("{" ~> repsep(expr, ",") <~ "}") ^^ {
    case className ~ attributesValues => NodeRecordInst(className, attributesValues)
  }

  private def fieldAccess: Parser[String] = "." ~> id

  // helper for expr0 to parse variable and recordInst (var is just "id" while recordInst is "id{expr, expr,...}")
  private def idBasedExpr: Parser[Node] = id ~ opt("{" ~> repsep(expr, ",") <~ "}") ^^ {
    case name ~ None => NodeVariable(name)
    case name ~ Some(values) => NodeRecordInst(name, values)
  }

  private def recordAccess: Parser[String] = "." ~> id

  // task 6: Loops
  private def loop: Parser[Node] = ("while" ~> expr) ~ ("do" ~> "{" ~> rep1(stmt <~ ";") <~ "}") ^^ {
    case condition ~ body => NodeWhile(condition, body)
  }


}

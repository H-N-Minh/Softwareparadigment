// This file contains the classes that represent the abstract syntax tree.
// You can change this as you like.

sealed trait Node

case class Program(funDecls: List[NodeFunctionDecl], main: Node) extends Node

case class NodeFunctionDecl(name: String, fun: NodeFunction) extends Node

case class NodeFunction(params: List[String], body: Node) extends Node

case class NodeCond(cond: Node, positive: Node, negative: Node) extends Node

case class NodeCall(fun: Node, params: List[Node]) extends Node

case class NodeVariable(name: String) extends Node

case class NodeValue(value: ExpValue) extends Node

case class NodePlus(lhs: Node, rhs: Node) extends Node

case class NodeMinus(lhs: Node, rhs: Node) extends Node

// TODO add nodes for all missing syntax elements

// task 1: List literals
case class NodeList(list: List[Node]) extends Node

// task 2: Multiplication and Comparison Operators
case class NodeMult(lhs: Node, rhs: Node) extends Node
case class NodeDiv(lhs: Node, rhs: Node) extends Node
case class NodeEq(lhs: Node, rhs: Node) extends Node
case class NodeNe(lhs: Node, rhs: Node) extends Node

// task 3: Sequence-Expressions
case class NodeSeq(stmts: List[Node], result: Node) extends Node
case class NodeVarDecl(name: String, value:Node) extends Node
case class NodeVarAssign(name: String, value: Node) extends Node

// task 4: Anonymous Functions
case class NodeFuncLit(params: List[String], body: Node) extends Node

// task 5: Records and Dot operator
case class NodeRecordDecl(className: String, attributesName: List[String]) extends Node
case class NodeRecordInst(className: String, attributesValues: List[Node]) extends Node
case class NodeFieldAccess(instance: Node, attributesName: String) extends Node
case class NodeFieldAssign(instance: Node, attributesName: String, attributesValues: Node) extends Node

// task 6: Loops
case class NodeWhile(condition: Node, body: List[Node]) extends Node


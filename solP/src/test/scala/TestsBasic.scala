import scala.collection.mutable._

class TestsBasic extends AbstractTests {

  test("Constants") {
    expectResult("True", ExpBoolean(true))
    expectResult("False", ExpBoolean(false))
    expectResult("0", ExpInteger(0))
    expectResult("12", ExpInteger(12))
    expectResult("-42", ExpInteger(-42))
    expectResult("Nil", ExpList(List()))
  }

  test("Builtin") {
    expectResult("and(True, True)", ExpBoolean(true))
    expectResult("and(True, False)", ExpBoolean(false))
    expectResult("or(True, True)", ExpBoolean(true))
    expectResult("or(False, True)", ExpBoolean(true))
    expectResult("or(False, False)", ExpBoolean(false))
    expectResult("not(True)", ExpBoolean(false))
    expectResult("not(False)", ExpBoolean(true))

    expectResult("add(1, (2))", ExpInteger(3))
    expectResult("sub(2, 4)", ExpInteger(-2))
    expectResult("mult(6, 7)", ExpInteger(42))
    expectResult("lt?(1, 2)", ExpBoolean(true))
    expectResult("lt?(2, 2)", ExpBoolean(false))
    expectResult("lt?(3, 2)", ExpBoolean(false))

    expectResult("build(1, Nil)", ExpList(List(ExpInteger(1))))
    expectResult("build(1, build(2, build(3, Nil)))", ExpList(List(ExpInteger(1), ExpInteger(2), ExpInteger(3))))
    expectResult("build(build(1, Nil), build(2, Nil))", ExpList(List(ExpList(List(ExpInteger(1))), ExpInteger(2))))
    expectResult("atom?(True)", ExpBoolean(true))
    expectResult("atom?(False)", ExpBoolean(true))
    expectResult("atom?(1)", ExpBoolean(true))
    expectResult("atom?(Nil)", ExpBoolean(false))
    expectResult("atom?(build(1, Nil))", ExpBoolean(false))
    expectResult("empty?(Nil)", ExpBoolean(true))
    expectResult("empty?(build(1, Nil))", ExpBoolean(false))
    expectResult("first(build(1, build(2, Nil)))", ExpInteger(1))
    expectResult("rest(build(1, Nil))", ExpList(List()))
    expectResult("rest(build(Nil, build(1, build(2, Nil))))", ExpList(List(ExpInteger(1), ExpInteger(2))))
  }

  test("Equality") {
    expectResult("eq?(2, add(1, 1))", ExpBoolean(true))
    expectResult("eq?(True, True)", ExpBoolean(true))
    expectResult("eq?(False, False)", ExpBoolean(true))
    expectResult("eq?(Nil, Nil)", ExpBoolean(true))
    expectResult("eq?(build(1, Nil), build(1, Nil))", ExpBoolean(true))

    expectResult("eq?(1, 2)", ExpBoolean(false))
    expectResult("eq?(True, False)", ExpBoolean(false))
    expectResult("eq?(Nil, build(Nil, Nil))", ExpBoolean(false))
    expectResult("eq?(build(1, build(2, Nil)), build(1, build(3, Nil)))", ExpBoolean(false))
  }

  test("Operator Simple") {
    expectResult("1+2", ExpInteger(3))
    expectResult("4-2", ExpInteger(2))
    expectResult("2+4-2+1-1", ExpInteger(4))
  }

  test("If Simple") {
    expectResult("if lt?(24, 42) then 12 else 21", ExpInteger(12))
    expectResult("if lt?(24, 24) then 12 else 21", ExpInteger(21))
    expectResult("if True then if eq?(sub(1, 2), 3) then 4 else 5 else 6", ExpInteger(5))
    expectResult("if True then if eq?(add(1, 2), 3) then 4 else 5 else 6", ExpInteger(4))
    expectResult("if False then if eq?(add(1, 2), 3) then 4 else 5 else 6", ExpInteger(6))
  }

  test("If Advanced") {
    expectResult("if if True then False else True then 1 else if False then 2 else 3", ExpInteger(3))
    expectResult("if if False then False else True then 1 else if False then 2 else 3", ExpInteger(1))
    expectResult("if if True then False else True then 1 else if True then 2 else 3", ExpInteger(2))
    expectResult("if not(and(if True then False else True, eq?(False, False))) then add(2, if False then 1 else 2) else 5", ExpInteger(4))
    expectResult("if not(and(if False then False else True, eq?(False, False))) then add(2, if False then 1 else 2) else 5", ExpInteger(5))
  }

  test("Function Simple") {
    expectResult(
      """
      $foo() => Nil;
      foo()
    """,
      ExpList(List()))
  }

  test("Function Constant") {
    expectResult(
      """
      $foo() => 1 + 2;
      $bar() => 3;
      add(foo(), bar())
    """,
      ExpInteger(6))
  }

  test("Function Parameters") {
    expectResult(
      """
      $foo(x, y) => add(x, y);
      $bar(x) => x;
      bar(foo(1 + bar(bar(1)), bar(3)))
    """,
      ExpInteger(5))
  }

  test("Function Recursion") {
    expectResult(
      """
      $even(x) => if eq?(x, 0) then True else not(even(sub(x, 1)));
      and(and(even(0), even(10)), and(not(even(1)), not(even(5))))
    """,
      ExpBoolean(true))

    expectResult(
      """
      $fibs(x) => if lt?(x, 2) then 1 else add(fibs(sub(x, 1)), fibs(sub(x, 2)));
      fibs(5)
    """,
      ExpInteger(8))
  }

  test("Function Mutual") {
    expectResult(
      """
      $foo(x, y) => add(x, bar(y));
      $bar(x) => if lorem(eq?(x, 0)) then x else 1;
      $lorem(ipsum) => not(ipsum);
      foo(bar(2), 0)
    """,
      ExpInteger(3))
  }

  test("Interpretation Order") {
    expectResult("add(add(print(1), print(2)), print(3))", ExpInteger(6), new Queue(), "123")

    expectResult(
      """
      $foo(i) => if eq?(print(i), 0) then print(2) else print(3);
      add(foo(0), foo(1))
    """,
      ExpInteger(5), new Queue(), "0213")
  }

  test("AD Example") {
    expectResult(
      """
      $addOne(i) => add(i, 1);
      $double(i) => mult(i, 2);
      $range(from, to, f) =>
        if lt?(from, to) then
          build(f(from), range(addOne(from), to, f))
        else
          Nil
        ;
      build(or(True, False), range(0, 5, double))
    """,
      ExpList(List(ExpBoolean(true), ExpInteger(0), ExpInteger(2), ExpInteger(4), ExpInteger(6), ExpInteger(8))))
  }
}

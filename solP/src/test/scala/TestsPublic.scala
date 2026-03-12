import scala.collection.mutable._

class TestsPublic extends AbstractTests {
  test("Lists") {
    expectResult("[]", ExpList(List()))
    expectResult("[1, 2]", ExpList(List(ExpInteger(1), ExpInteger(2))))
    expectResult("[[print(1), print(2)], print(3)]", ExpList(List(ExpList(List(ExpInteger(1), ExpInteger(2))), ExpInteger(3))), new Queue(), "123")
  }

  test("Operators") {
    expectResult("2 * 4", ExpInteger(8))
    expectResult("4 + 4 / 2", ExpInteger(6))
    expectResult("10 - 2 * 5", ExpInteger(0))
    expectResult("10 == 2 * 5", ExpBoolean(true))
    expectResult("2 * 3 == 10 - 3", ExpBoolean(false))
    expectResult("4 / 4 != 0 + 2", ExpBoolean(true))

    expectResult("8 / 4 * 2 / 1", ExpInteger(4))
    expectResult("3 + 2 * 3 == 18 / 2", ExpBoolean(true))
    expectResult("[1, 2] + [3, 4]", ExpList(List(ExpInteger(1), ExpInteger(2), ExpInteger(3), ExpInteger(4))))
  }

  test("Sequence Expressions") {
    expectResult(
      """
      {
        $a := 10;
        $b := add(a, 42);
        add(a,b)
      }
    """, ExpInteger(62))

    expectResult(
      """
      {
        $a := 4;
        a  := add(a, 20);
        a
      }
    """, ExpInteger(24))

    expectResult(
      """
      {
        $a := 1;
        $b := 4;
        {
          $a := 20;
          $c := a + b;
          c
        } + a
      }
    """, ExpInteger(25))

    expectResult(
      """
      {
        $a := 4;
        $b := 2;
        a  := {
                $a := 10;
                $b := a + b;
                b
              };
        a + b
      }
    """, ExpInteger(14))

    expectResult(
      """
      {
        $a := 4;
        $b := {
  	            a := 20;
  	            a
              };
        a + b
      }
    """, ExpInteger(40))



    // new Test: Sequence with just a final expression (no statements)
    expectResult("{ 42 }", ExpInteger(42))

    // new Test: Sequence as an operand (in an expression)
    expectResult("add({ $a := 5; a }, 10)", ExpInteger(15))

    // new Test: 3 nested sequences
    expectResult("""
      {
        $x := 5;
        {
          $y := 10;
          {
            $z := 15;
            add(x, add(y, z))
          }
        }
      }
    """, ExpInteger(30))
  }



  test("Anonymous Functions") {
    expectResult(
      """
      ((i) => i + 4)(2)
    """, ExpInteger(6))

    expectResult(
      """
      ((a, b, c) => a + b - c)(3,2,1)
    """, ExpInteger(4))

    expectResult(
      """
      (() => 4 - 2)()
    """, ExpInteger(2))

    expectResult(
      """
      ((a) => build(a, build(a + 1, Nil)))(2)
    """, ExpList(List(ExpInteger(2), ExpInteger(3))))

    expectResult(
      """
      ((x) => first(x))(build(1,build(2,build(3, Nil))))
    """, ExpInteger(1))

    expectResult(
      """
      {
        $a   := 1;
        $b   := 5;
        $fun := {
                  $a := 3;
                  $c := 10;
                  (b) => a + b + c
                };
        $foo := {
                  $a := 4;
                  fun(2)
                };
        foo
      }
    """, ExpInteger(15))

    expectResult(
      """
      {
        $apply := (x, f) => f(x);
        apply(4, (x) => x * 2)
      }
    """, ExpInteger(8))

    expectResult(
      """
      {
        $mul    := (a) => (b) => a * b;
        $times2 := mul(2);
        $times4 := mul(4);
        times2(5) + times4(7)
      }
    """, ExpInteger(38))
  }

  test("Records") {
    expectResult(
      """
      $foo{a, b, c};

      {
        $fooObj := foo{1, 4, 2 + 10};
        fooObj.c
      }
    """, ExpInteger(12))

    expectResult(
      """
      $foo{a, b, c};

      {
        $fooCtor := () => foo{1, 4, 2};
        $getB := (obj) => obj.b;
        getB(fooCtor())
      }
    """, ExpInteger(4))

    expectResult(
      """
      $foo{a, b};

      {
        $fooObj := foo{1, 4};
        fooObj.a := fooObj.b + 20;
        fooObj.a
      }
    """, ExpInteger(24))
  }

  test("Loops") {
    expectResult(
      """
      {
        $x   := 2;
        $y   := 8;
        $res := 1;
        while y != 0 do {
          res := res * x;
          y := y - 1;
        };
        res
      }
    """, ExpInteger(256))

    expectResult(
      """
      {
        $ls  := [1, 4, 2];
        $tmp := 0;
        while not(empty?(ls)) do {
          tmp := print(first(ls));
          ls  := rest(ls);
        };
        ls
      }
    """, ExpList(List()), new Queue(), "142")

    // Test: Loop with complex condition (using 'and')
    expectResult("""
      {
        $a := 5;
        $b := 10;
        while and(a != b, b != 0) do {
          a := a + 1;
          b := b - 1;
        };
        a
      }
    """, ExpInteger(15))

    // Test: 2 Nested loops
    expectResult("""
      {
        $i := 0;
        $sum := 0;
        while i != 3 do {
          $j := 0;
          while j != 2 do {
            sum := sum + 1;
            j := j + 1;
          };
          i := i + 1;
        };
        sum
      }
    """, ExpInteger(6))


    // Test: Loop that is skipped because cond is false 
    expectResult("""
      {
        $i := 10;
        $sum := 0;
        while i != 10 do {
          sum := sum + i;
          i := i + 1;
        };
        sum
      }
    """, ExpInteger(0))
  }

  test("Misc") {
    expectResult(
      """
      {
        $a := 1;
        $b := a;
        $c := {
                $a := 2;
                $c := a != b;
                c
              };
        c
      }
    """, ExpBoolean(true))

    expectResult(
      """
      {
        $area5 := (a,c) => a / 2 + c / 2;
        $area1 := (a,c,h) => area5(a,c)*h;
        area1(4,6,2)
      }
    """, ExpInteger(10))

    expectResult(
      """
      {
        $area      := (c) => (a,b) => a * b / c;
        $rectangle := area(1);
        $square    := (l) => rectangle(l,l);
        $triangle  := area(2);
        $rect2     := (l) => rectangle(2,l);
        $tri2      := (l) => triangle(2,l);
        $list      := [square(2), rect2(4), tri2(3)];
        list
      }
    """, ExpList(List(ExpInteger(4), ExpInteger(8), ExpInteger(3))))

    expectResult(
      """
      {
        $area      := (c) => (a,b) => a * b / c;
        $rectangle := area(1);
        $square    := (l) => rectangle(l,l);
        $triangle  := area(2);
        $rect2     := (l) => rectangle(2,l);
        $tri2      := (l) => triangle(2,l);
        $list      := [square(2), rect2(4), tri2(3)];
        list
      }
    """, ExpList(List(ExpInteger(4), ExpInteger(8), ExpInteger(3))))
  }
}

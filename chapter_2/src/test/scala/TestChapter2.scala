import org.junit.Test
import org.junit.Assert.*

class TestChapter2:
  @Test def fib_test(): Unit =
    assertEquals(fib(0),0)
    assertEquals(fib(1),1)
    assertEquals(fib(2),1)
    assertEquals(fib(3),2)
    assertEquals(fib(4),3)
    assertEquals(fib(5),5)
    assertEquals(fib(6),8)

  @Test def isSorted_test(): Unit =
    assertTrue(isSorted(List(), (a: Char, b: Char) => a <= b))
    assertTrue(isSorted(List('a'), (a: Char, b: Char) => a <= b))
    assertTrue(isSorted(List('a', 'b', 'c'), (a: Char, b: Char) => a <= b))
    assertTrue(isSorted(List('a', 'b', 'b'), (a: Char, b: Char) => a <= b))
    assertFalse(isSorted(List('a', 'c', 'b'), (a: Char, b: Char) => a <= b))

  @Test def currying_test(): Unit =
    val add = (a:Int, b:Int) => a+b

    val addCurried = curry(add)
    assertEquals(addCurried(2)(10),12)

    val add2 = addCurried(2)
    assertEquals(add2(10),12)

    val addUncurried = uncurry(addCurried)
    assertEquals(addUncurried(2,10), 12)

  @Test def compose_test(): Unit =
    val square = (a:Int) => a*a
    val by10 = (a:Int) => a*10
    val square_by10 = compose(by10,square)
    assertEquals(by10(square(2)),square_by10(2))

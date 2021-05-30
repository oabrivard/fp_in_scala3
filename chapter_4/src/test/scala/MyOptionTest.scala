import org.junit.Test
import org.junit.Assert.*

class MyOptionTest:
  @Test def variance_test(): Unit =
    val l = List(-5.0, -4.0, -4.0, -3.0, -3.0, -2.0, -1.0, 0.0, 0.0, 1.0, 2.0, 3.0, 3.0, 4.0, 4.0, 6.0, 7.0, 8.0, 9.0, 10.0, 10.0, 11.0, 11.0, 12.0)
    assertEquals(30.04, MyOption.variance(l).getOrElse(()=>0.0), 0.01)

  @Test def traverse_test(): Unit =
    val xs = List(1,2,3,4)
    val result = MyOption.traverse(xs, (x:Int) => MyOption.Some(x*x))
    assertEquals(MyOption.Some(List(1,4,9,16)),result)
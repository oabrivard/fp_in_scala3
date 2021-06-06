import org.junit.Test
import org.junit.Assert.*

class MyStreamTest:
  @Test def toList(): Unit =
    val s = MyStream.apply(1,2,3)
    assertEquals(List(1,2,3), s.toList)

  @Test def take(): Unit =
    val s = MyStream.apply(1,2,3,4)
    assertEquals(List(1,2), s.take(2).toList)

  @Test def drop(): Unit =
    val s = MyStream.apply(1,2,3,4)
    assertEquals(List(3,4), s.drop(2).toList)

  @Test def takeWhile(): Unit =
    val s = MyStream.apply(1,2,3,4)
    assertEquals(List(1,2), s.takeWhile(_<3).toList)

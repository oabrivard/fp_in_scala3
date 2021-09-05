package fpinscala3.chapter_5

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

  @Test def headOption2(): Unit =
    val s1 = MyStream.apply(1,2,3,4)
    assertEquals(Some(1), s1.headOption2)
    val s2 = MyStream.empty[Int]
    assertEquals(Option.empty[Int], s2.headOption2)

  @Test def map(): Unit =
    val s = MyStream.apply(1,2,3,4).map(_*2)
    assertEquals(List(2,4,6,8), s.toList)

  @Test def filter(): Unit =
    val s = MyStream.apply(1,2,3,4).filter(_%2==0)
    assertEquals(List(2,4), s.toList)

  @Test def constant(): Unit =
    val s = MyStream.constant(3107).take(3)
    assertEquals(List(3107,3107,3107), s.toList)

  @Test def from(): Unit =
    val s = MyStream.from(3107).take(3)
    assertEquals(List(3107,3108,3109), s.toList)

  @Test def fibs(): Unit =
    val s = MyStream.fibs.take(7)
    assertEquals(List(1,1,2,3,5,8,13), s.toList)

  @Test def fibs2(): Unit =
    val s = MyStream.fibs2.take(7)
    assertEquals(List(1,1,2,3,5,8,13), s.toList)

  @Test def from2(): Unit =
    val s = MyStream.from2(3107).take(3)
    assertEquals(List(3107,3108,3109), s.toList)

  @Test def map2(): Unit =
    val s = MyStream.apply(1,2,3,4).map2(_*2)
    assertEquals(List(2,4,6,8), s.toList)

  @Test def take2(): Unit =
    val s = MyStream.apply(1,2,3,4)
    assertEquals(List(1,2), s.take2(2).toList)

  @Test def takeWhile3(): Unit =
    val s = MyStream.apply(1,2,3,4)
    assertEquals(List(1,2), s.takeWhile3(_<3).toList)

  @Test def zipWith(): Unit =
    val s1 = MyStream.apply(1,2,3,4)
    val s2 = MyStream.apply(1,2,3)
    val s = MyStream.zipWith(s1,s2,(_,_))
    assertEquals(List((1,1),(2,2),(3,3)), s.toList)

  @Test def zipAll(): Unit =
    val s1 = MyStream.apply(1,2,3,4)
    val s2 = MyStream.apply(1,2,3)
    val s = MyStream.zipAll(s1,s2)
    assertEquals(List((Some(1),Some(1)), (Some(2),Some(2)), (Some(3),Some(3)), (Some(4),None)), s.toList)

  @Test def startWith(): Unit =
    val s1 = MyStream.apply(1,2,3,4)
    val s2 = MyStream.apply(1,2,3)
    assertTrue(s1.startsWith(s2))
    assertFalse(s2.startsWith(s1))

  @Test def tail(): Unit =
    val s = MyStream.apply(1,2,3)
    assertEquals(List(List(1,2,3), List(2,3), List(3), List()), s.tails.map(_.toList).toList)

  @Test def scanRight(): Unit =
    val s = MyStream(1,2,3)
    assertEquals(List(6,5,3,0), s.scanRight(0)(_+_).toList)

  @Test def scanRight2(): Unit =
    val s = MyStream(1,2,3)
    assertEquals(List(6,5,3,0), s.scanRight2(0)(_+_).toList)

import org.junit.Test
import org.junit.Assert.*

class TestChapter3:
  @Test def list_of(): Unit =
    val l1 = MyList.of(1,2,3)
    assertEquals(MyList.Cons(1,MyList.Cons(2, MyList.Cons(3, MyList.Nil))), l1)

  @Test def list_sum(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val sum = MyList.sum(l1)
    assertEquals(10,sum)
    assertEquals(0,MyList.sum(MyList.Nil))

  @Test def list_product(): Unit =
    val l1 = MyList.of(1.0,2.0,3.0,4.0)
    val product = MyList.product(l1)
    assertEquals(24.0,product,0.0000001)
    assertEquals(0.0,MyList.product(MyList.Nil),0.0000001)

  @Test def list_append(): Unit =
    val l1 = MyList.of(1,2)
    val l2 = MyList.of(3,4)
    val l3 = MyList.of(1,2,3,4)
    assertEquals(l3,MyList.append(l1,l2))

  @Test def tail(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val l2 = l1.tail
    assertEquals(l2, MyList.of(2,3,4))

  @Test def setHead(): Unit =
    val l1 = MyList.of(2,3,4)
    val l2 = l1.setHead(1)
    assertEquals(l2, MyList.of(1,2,3,4))

  @Test def drop(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val l2 = l1.drop(2)
    assertEquals(l2, MyList.of(3,4))
    assertEquals(l1.drop(0), MyList.of(1,2,3,4))
    assertEquals(MyList.Nil.drop(0), MyList.Nil)

  @Test def dropWhile(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val l2 = l1.dropWhile(_ < 3)
    assertEquals(l2, MyList.of(3,4))
    assertEquals(l1.dropWhile(_ < 5), MyList.Nil)
    assertEquals(l1.dropWhile(_ < 0), l1)

  @Test def init(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val l2 = l1.init()
    assertEquals(l2, MyList.of(1,2,3))
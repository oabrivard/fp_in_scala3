package fpinscala3.chapter_3

import org.junit.Test
import org.junit.Assert.*

class MyListTest:
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

  @Test def list_foldRight(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val sum = MyList.foldRight(l1, 0, (x,y) => x+y)
    assertEquals(10,sum)

  @Test def length(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(4, l1.length)
    assertEquals(0, MyList.Nil.length)

  @Test def list_foldLeft(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val sum = MyList.foldLeft(l1, 0, (x,y) => x+y)
    assertEquals(10,sum)

  @Test def list_lengthWithFoldLeft(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(4, MyList.lengthWithFoldLeft(l1))
    assertEquals(0, MyList.lengthWithFoldLeft(MyList.Nil))

  @Test def list_sumWithFoldLeft(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(10, MyList.sumWithFoldLeft(l1))
    assertEquals(0, MyList.sumWithFoldLeft(MyList.Nil))

  @Test def list_productWithFoldLeft(): Unit =
    val l1 = MyList.of(1.0,2.0,3.0,4.0)
    val product = MyList.productWithFoldLeft(l1)
    assertEquals(24.0,product,0.0000001)
    assertEquals(0.0,MyList.productWithFoldLeft(MyList.Nil),0.0000001)

  @Test def reverse(): Unit =
    val l1 = MyList.of(1,2,3,4)
    val l2 = MyList.of(4,3,2,1)
    assertEquals(l2, l1.reverse())
    assertEquals(MyList.Nil, MyList.Nil.reverse())

  @Test def List_foldRightWithFoldLeft(): Unit =
    val l1 = MyList.of('a','b','c')
    val concat = MyList.foldRightWithFoldLeft(l1, "_", (x,y) => x.toString()+y)
    assertEquals("abc_",concat)

  @Test def List_foldLeftWithFoldRight(): Unit =
    val l1 = MyList.of('a','b','c')
    val concat = MyList.foldLeftWithFoldRight(l1, "_", (x,y) => x+y)
    assertEquals("_abc",concat)

  @Test def list_appendWithFold(): Unit =
    val l1 = MyList.of(1,2)
    val l2 = MyList.of(3,4)
    val l3 = MyList.of(1,2,3,4)
    assertEquals(l3,MyList.appendWithFold(l1,l2))

  @Test def list_concatenate(): Unit =
    val l1 = MyList.of(MyList.of(1,2),MyList.of(3,4))
    val l2 = MyList.of(1,2,3,4)
    assertEquals(l2,MyList.concatenate(l1))

  @Test def list_transformInts(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(MyList.of(2,3,4,5), MyList.transformInts(l1))

  @Test def list_transformDouble(): Unit =
    val l1 = MyList.of(1.0,2.0,3.0,4.0)
    assertEquals(MyList.of("1.0","2.0","3.0","4.0"), MyList.transformDouble(l1))

  @Test def list_map(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(MyList.of(2,4,6,8), MyList.map(l1,_*2))

  @Test def list_map_2(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertEquals(MyList.of(2,4,6,8), MyList.map_2(l1,_*2))

  @Test def list_filter(): Unit =
    val l1 = MyList.of(1,2,3,4,5,6)
    assertEquals(MyList.of(2,4,6), MyList.filter(l1,_%2==0))

  @Test def list_flatMap(): Unit =
    val l1 = MyList.of(1,2,3)
    assertEquals(MyList.of(1,1,2,2,3,3), MyList.flatMap(l1,(i)=>MyList.of(i,i)))

  @Test def list_filterWithFlatMap(): Unit =
    val l1 = MyList.of(1,2,3,4,5,6)
    assertEquals(MyList.of(2,4,6), MyList.filterWithFlatMap(l1,_%2==0))

  @Test def list_addListOfInts(): Unit =
    val l1 = MyList.of(1,2,3)
    val l2 = MyList.of(4,5,6)
    assertEquals(MyList.of(5,7,9), MyList.addListOfInts(l1,l2))

  @Test def list_zipWith(): Unit =
    val l1 = MyList.of(1,2,3)
    val l2 = MyList.of(4,5,6)
    assertEquals(MyList.of(5,7,9), MyList.zipWith(l1,l2,_+_))

  @Test def list_startWith(): Unit =
    val l1 = MyList.of(1,2,3,4,5)
    val l2 = MyList.of(1,2,3)
    val l3 = MyList.of(2,3,4)
    val l4 = MyList.of(1,2,3,4,5,6)
    assertTrue(MyList.startWith(l1,l2))
    assertFalse(MyList.startWith(l1,l3))
    assertFalse(MyList.startWith(l1,l4))

  @Test def list_hasSubsequence(): Unit =
    val l1 = MyList.of(1,2,3,4)
    assertTrue(MyList.hasSubsequence(l1,MyList.of(1,2,3)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(2,3,4)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(1,2)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(2,3)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(3,4)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(1)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(2)))
    assertTrue(MyList.hasSubsequence(l1,MyList.of(3)))
    assertTrue(MyList.hasSubsequence(l1,MyList.Nil))
    assertFalse(MyList.hasSubsequence(l1,MyList.of(1,2,3,4,5)))
    assertFalse(MyList.hasSubsequence(l1,MyList.of(2,4,3)))


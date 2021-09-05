package fpinscala3.chapter_3

import org.junit.Test
import org.junit.Assert.*

class MyTreeTest:
  @Test def size(): Unit =
    val t = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    assertEquals(5, t.size)

  @Test def tree_maximum(): Unit =
    val t = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    assertEquals(5, MyTree.maximum(t))

  @Test def depth(): Unit =
    val t = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    assertEquals(3, t.depth)

  @Test def map(): Unit =
    val t1 = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    val t2 = MyTree.Node(MyTree.Node(MyTree.Leaf(2),MyTree.Leaf(3)),MyTree.Node(MyTree.Node(MyTree.Leaf(4),MyTree.Leaf(5)),MyTree.Leaf(6)))
    assertEquals(t2, t1.map(_+1))

  @Test def fold(): Unit =
    val t = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    assertEquals(5, t.fold(_=>1,(l,r) => l+r))  //size
    assertEquals(5, t.fold(identity,(l,r) => math.max(l,r))) //maximum
    assertEquals(3, t.fold(_=>0,(l,r) => math.max(l,r)+1)) //depth

  @Test def mapWithFold(): Unit =
    val t1 = MyTree.Node(MyTree.Node(MyTree.Leaf(1),MyTree.Leaf(2)),MyTree.Node(MyTree.Node(MyTree.Leaf(3),MyTree.Leaf(4)),MyTree.Leaf(5)))
    val t2 = MyTree.Node(MyTree.Node(MyTree.Leaf(2),MyTree.Leaf(3)),MyTree.Node(MyTree.Node(MyTree.Leaf(4),MyTree.Leaf(5)),MyTree.Leaf(6)))
    assertEquals(t2, t1.mapWithFold(_+1))

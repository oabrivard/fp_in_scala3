package fpinscala3.chapter_7

import org.junit.Test
import org.junit.Assert.*

class ParallelTest:
  @Test def sum(): Unit =
    val parSum = Par.sum(IndexedSeq(1,2,3,4))
    val a = Par.run(Par.sequentialExecutor())(parSum)
    assertEquals(10, a.get)

  @Test def max(): Unit =
    val parMax = Par.max(IndexedSeq(1,2,7,4))
    val a = Par.run(Par.sequentialExecutor())(parMax)
    assertEquals(7, a.get)

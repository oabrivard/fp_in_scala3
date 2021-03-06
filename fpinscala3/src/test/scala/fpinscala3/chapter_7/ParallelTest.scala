package fpinscala3.chapter_7

import org.junit.Test
import org.junit.Assert.*

import java.util.concurrent.{ExecutorService, Executors}

class ParallelTest:
  @Test def sum(): Unit =
    val ES: ExecutorService = Executors.newCachedThreadPool
    val parSum = Par.sum(IndexedSeq(1,2,3,4))
    val a = Par.run(ES)(parSum)
    assertEquals(10, a.get)

  @Test def max(): Unit =
    val ES: ExecutorService = Executors.newCachedThreadPool
    val parMax = Par.max(IndexedSeq(1,2,7,4))
    val a = Par.run(ES)(parMax)
    assertEquals(7, a.get)

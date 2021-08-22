import org.junit.Test
import org.junit.Assert.*

class ParallelTest:
  @Test def t1(): Unit = 
    assertEquals("I was compiled by Scala 3. :)", "")

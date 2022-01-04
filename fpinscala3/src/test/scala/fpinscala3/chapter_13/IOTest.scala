package fpinscala3.chapter_13

import org.junit.Test
import org.junit.Assert.*

class IOTest:
  @Test def C2FTest(): Unit =
    val io2 = converter2
    io2.run
    val io3 = converter3
    io3.run
    val t = test
    println("X")
    t.run

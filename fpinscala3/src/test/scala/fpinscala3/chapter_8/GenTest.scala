package fpinscala3.chapter_8

import fpinscala3.chapter_6.{RNG, SimpleRNG}
import org.junit.Test
import org.junit.Assert.*

class GenTest:
  @Test def ChooseTest(): Unit =
    val rng1 = SimpleRNG(3107)
    val gen = Gen.choose(10,100)
    val (i,rng2) = gen.sample.run(rng1)
    assertTrue(i >= 10 && i < 100)

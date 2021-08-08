import org.junit.Test
import org.junit.Assert.*

class TestRNG:
  @Test def nonNegativeInt(): Unit =
    val rng = SimpleRNG(3107)
    assertTrue(RNG.nonNegativeInt(rng)._1 >= 0)

  @Test def sequence(): Unit =
    val l = List(RNG.unit(1), RNG.unit(2), RNG.unit(3))
    val s = RNG.sequence(l)
    val rng = SimpleRNG(3107)
    assertEquals(List(1,2,3),s(rng)._1)

  @Test def sequenceWithFold(): Unit =
    val s = RNG.sequenceWithFold(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
    val rng = SimpleRNG(3107)
    assertEquals(List(1,2,3),s(rng)._1)

import org.junit.Test
import org.junit.Assert.*

class TestRNG:
  @Test def nonNegativeInt(): Unit =
    val rng = SimpleRNG(3107)
    assertTrue(RNG.nonNegativeInt(rng)._1 >= 0)

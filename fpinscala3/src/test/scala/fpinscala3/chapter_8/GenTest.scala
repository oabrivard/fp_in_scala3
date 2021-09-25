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

  @Test def Max: Unit =
    val smallInt = Gen.choose(-10,10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)

  @Test def Sort: Unit =
    val smallInt = Gen.choose(-10,10)
    val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      (sorted.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists {
        case (a,b) => a > b })
    }
    Prop.run(sortedProp)

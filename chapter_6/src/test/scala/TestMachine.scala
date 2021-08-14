import org.junit.Test
import org.junit.Assert.*

class TestMachine:
  @Test def simulateMachine(): Unit =
    val m = Machine(true,5,10)
    val inputs = List(Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Turn)
    val ms = Candy.simulateMachine(inputs)
    val ((coins,candies),_) = ms.run(m)
    assertEquals(14,coins)
    assertEquals(1,candies)

  @Test def simulateMachine2(): Unit =
    val m = Machine(true,5,10)
    val inputs = List(Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Turn, Input.Coin, Input.Turn)
    val ms = Candy.simulateMachine2(inputs)
    val ((coins,candies),_) = ms.run(m)
    assertEquals(14,coins)
    assertEquals(1,candies)

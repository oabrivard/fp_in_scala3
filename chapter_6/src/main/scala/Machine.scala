enum Input:
  case Coin
  case Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    def simulateOne(m:Machine, i:Input): Machine = i match
      case Input.Coin if m.locked && m.candies > 0 => Machine(false,m.candies,m.coins+1)
      case Input.Turn if !m.locked && m.candies > 0 => Machine(true,m.candies-1,m.coins)
      case _ => m
      /*
      Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
      Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
      A machine that’s out of candy ignores all inputs.
      */

    State(m1 => {
      val m3 = inputs.foldLeft(m1){(m2,i) => simulateOne(m2,i)}
      ((m3.coins,m3.candies), m3)
    })

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(
      inputs.map(
        i => State.modify(
          (s: Machine) => (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Input.Coin, Machine(false, _, _)) => s
            case (Input.Turn, Machine(true, _, _)) => s
            case (Input.Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
            case (Input.Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
          }
        )
      )
    )
    s <- State.get
  } yield (s.coins, s.candies)



//type State[S,+A] = S => (A,S)

case class State[S,+A](run: S => (A,S)):
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B): State[S,B] = flatMap(a => State.unit(f(a)))

object State:
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] =
    sa.flatMap { a =>
      sb.map { b =>
        f(a,b)
      }
    }

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit(List[A]())) {(sa,sla) => map2(sa,sla) {_::_}}

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

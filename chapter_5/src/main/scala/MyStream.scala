enum MyStream[+A]:
  case Empty
  case Cons(h: () => A, t: () => MyStream[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, t) => Some(h())

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
    /*
    @annotation.tailrec
    def loop(s:MyStream[A], acc: List[A]): List[A] = s match
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h()::acc)

    loop(this, Nil)
    */

  def take(n: Int): MyStream[A] = this match
    case Cons(h,t) if n > 1 => MyStream.cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => MyStream.cons(h(), MyStream.Empty)
    case _ => MyStream.empty

  @annotation.tailrec
  final def drop(n: Int): MyStream[A] = this match
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this

  def takeWhile(p: A => Boolean): MyStream[A] = this match
    case Cons(h,t) if p(h()) => MyStream.cons(h(), t().takeWhile(p))
    case _ => MyStream.empty

object MyStream:
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] =
    lazy val head = hd
    lazy val tail = tl
    MyStream.Cons(() => head, () => tail)

  def empty[A]: MyStream[A] = MyStream.Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


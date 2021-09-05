package fpinscala3.chapter_5

enum MyStream[+A]:
  case Empty
  case Cons(h: () => A, t: () => MyStream[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, t) => Some(h())

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  /*
    @annotation.tailrec
    def loop(s:MyStream[A], acc: List[A]): List[A] = s match
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h()::acc)

    loop(this, Nil)
    */

  def take(n: Int): MyStream[A] = this match
    case Cons(h, t) if n > 1 => MyStream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => MyStream.cons(h(), MyStream.empty)
    case _ => MyStream.empty

  @annotation.tailrec
  final def drop(n: Int): MyStream[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): MyStream[A] = this match
    case Cons(h, t) if p(h()) => MyStream.cons(h(), t().takeWhile(p))
    case _ => MyStream.empty

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty)((a, b) => if p(a) then MyStream.cons(a, b) else MyStream.empty)

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): MyStream[B] = foldRight(MyStream.empty[B]) { (a, b) => MyStream.cons(f(a), b) }

  def filter(p: A => Boolean): MyStream[A] = foldRight(MyStream.empty[A]) { (a, b) => if p(a) then MyStream.cons(a, b) else b }

  def append[B>:A](s: => MyStream[B]): MyStream[B] = foldRight(s) { (a, b) => MyStream.cons(a, b) }

  def flatMap[B](f: A => MyStream[B]) : MyStream[B] = foldRight(MyStream.empty[B]) { (a,b) => f(a).append(b) }

  def map2[B](f: A => B): MyStream[B] = MyStream.unfold(this) {
    case Cons(h,t) => Some((f(h()),t()))
    case _ => None
  }

  def take2(n: Int): MyStream[A] = MyStream.unfold((this,n)) {
    case (Cons(h, _),1) => Some((h(), (MyStream.empty,0)))
    case (Cons(h, t),n) if n > 1 => Some((h(), (t(),n-1)))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): MyStream[A] = MyStream.unfold(this) {
    case Cons(h,t) if p(h()) => Some((h(),t()))
    case _ => None
  }

  def startsWith[A](s: MyStream[A]): Boolean =
    MyStream.zipAll(this,s).takeWhile(!_._2.isEmpty).forAll((o1,o2) => o1==o2)
    //MyStream.zipAll(this,s).forAll((o1,o2) => o1==o2 || o2==None)

  def tails: MyStream[MyStream[A]] = MyStream.unfold(this) {
    case Cons(h,t) => Some(MyStream.cons(h(),t()), t())
    case _ => None
  } append MyStream.apply(MyStream.empty)

  /*
  def tails2: MyStream[MyStream[A]] = MyStream.unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append MyStream(MyStream.empty)
   */

  def hasSubsequence[A](s: MyStream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): MyStream[B] = tails map {s => s.foldRight(z)(f)}
  def scanRight2[B](z: => B)(f: (A, => B) => B): MyStream[B] =
    foldRight((MyStream(z),z))((a,p) => {
      val (s,last) = p
      lazy val next = f(a,last)
      (MyStream.cons(next,s),next)
    })._1

object MyStream:
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

  def from(n: Int): MyStream[Int] = cons(n, from(n+1))

  def fibs: MyStream[Int] =
    def fibo(n1: Int, n2: Int): MyStream[Int] = cons(n1, fibo(n2, n1+n2))
    fibo(1,1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match
    case Some((a,s)) => cons(a,unfold(s)(f))
    case None => empty

  def fibs2: MyStream[Int] = unfold((1,1))((a,b) => Some((a,(b,a+b))))

  def from2(n: Int): MyStream[Int] = unfold(n)(s => Some((s,s+1)))

  def constant2[A](a: A): MyStream[A] = unfold(a)(_ => Some((a,a)))

  def ones: MyStream[Int] = unfold(1)(_ => Some((1,1)))

  def zipWith[A,B,C](xs1:MyStream[A],xs2:MyStream[B], f: (A,B)=>C): MyStream[C] = unfold((xs1,xs2)) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
    case _ => None
  }

  def zipAll[A,B](s1: MyStream[A], s2: MyStream[B]): MyStream[(Option[A],Option[B])] = unfold((s1,s2)) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some(((Some(h1()),Some(h2())), (t1(),t2())))
    case (Empty,Cons(h2,t2)) => Some(((None,Some(h2())), (Empty,t2())))
    case (Cons(h1,t1),Empty) => Some(((Some(h1()),None), (t1(),Empty)))
    case _ => None
  }

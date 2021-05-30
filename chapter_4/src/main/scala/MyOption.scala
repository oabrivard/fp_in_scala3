enum MyOption[+A]:
  case None
  case Some(value: A)

extension [A](o: MyOption[A])
  def map[B](f: A => B): MyOption[B] = o match
    case MyOption.Some(value) => MyOption.Some(f(value))
    case MyOption.None => MyOption.None

  def getOrElse(default: () => A): A = o match
    case MyOption.Some(value) => value
    case MyOption.None => default()

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = o.map(f).getOrElse(() => MyOption.None)
    /*
    o match
    case MyOption.Some(value) => f(value)
    case MyOption.None => MyOption.None
    */

  def filter(f: A => Boolean): MyOption[A] = o.flatMap(a => if f(a) then MyOption.Some(a) else MyOption.None)

  def orElse(ob: () => MyOption[A]): MyOption[A] = o.map(MyOption.Some(_)).getOrElse(ob)
    /*
    o match
    case MyOption.Some(_) => o
    case MyOption.None => ob()
    */
  /*
  fun <A, B> Option<A>.flatMap(f: (A) -> Option<B>): Option<B> = TODO()❷

  fun <A> Option<A>.orElse(ob: () -> Option<A>): Option<A> = TODO() ❹

    fun <A> Option<A>.filter(f: (A) -> Boolean): Option<A> = TODO() ❺

  */

object MyOption:
  def mean(xs: List[Double]): MyOption[Double] = if (xs.isEmpty) MyOption.None else MyOption.Some(xs.sum / xs.size)

  def variance(xs: List[Double]): MyOption[Double] = mean(xs).flatMap(m => if (xs.size <= 1) MyOption.None else MyOption.Some(xs.map(x => math.pow(x - m,2)).sum / (xs.size-1)))

  def lift[A,B](f:A=>B) : MyOption[A]=>MyOption[B] = _.map(f)

  def map2[A,B,C](oa:MyOption[A], ob:MyOption[B], f: (A,B) => C): MyOption[C] =
    for
      a <- oa
      b <- ob
    yield
      f(a,b)
    //oa.flatMap((a:A) => ob.map((b:B) => f(a,b)))

  def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] =
    xs.foldRight(MyOption.Some(Nil))((oa: MyOption[A], ol:MyOption[List[A]]) => map2(oa, ol, (a,l) => a::l))
    //xs.foldRight(MyOption.Some(Nil))((oa: MyOption[A], ol:MyOption[List[A]]) => ol.flatMap(l => oa.map(a => a::l)))
    /*
    xs match
    case Nil => MyOption.Some(Nil)
    case ox::oxs => sequence(oxs).flatMap(xs => ox.map(x => x::xs))
    */

  def traverse[A,B](xa: List[A], f: (A) => MyOption[B]): MyOption[List[B]] =
    xa.foldRight(MyOption.Some(Nil))( (a: A, ol:MyOption[List[B]]) => map2(f(a),ol,(a,l) => a::l) )
    //sequence(xa.map(f))

  def sequence2[A](xs: List[MyOption[A]]): MyOption[List[A]] = traverse(xs,identity)

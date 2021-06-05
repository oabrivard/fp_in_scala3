import MyOption.traverse

enum MyEither[+E,+A]:
  case Left(value: E)
  case Right(value: A)

extension [E,A](e: MyEither[E,A])
  def map[B](f: A => B): MyEither[E,B] = e match
    case MyEither.Left(value) => MyEither.Left(value)
    case MyEither.Right(value) => MyEither.Right(f(value))

  def flatMap[B](f: A => MyEither[E,B]): MyEither[E,B] = e match
    case MyEither.Left(value) => MyEither.Left(value)
    case MyEither.Right(value) => f(value)

  def orElse(f: ()=> MyEither[E,A]): MyEither[E,A] = e match
    case MyEither.Left(value) => f()
    case MyEither.Right(value) => MyEither.Right(value)

object MyEither:
  def map2[E,A,B,C](ea:MyEither[E,A], eb:MyEither[E,B], f: (A,B) => C): MyEither[E,C] =
    ea.flatMap(a => eb.map(b => f(a,b) ))

  def sequence[E,A](xs: List[MyEither[E,A]]): MyEither[E,List[A]] =
    xs.foldRight(MyEither.Right(Nil))((ea: MyEither[E,A],el: MyEither[E,List[A]]) => map2(ea,el, (a,l) => a::l))

  def traverse[E,A,B](xa: List[A], f: (A) => MyEither[E,B]): MyEither[E,List[B]] =
    xa.foldRight(MyEither.Right(Nil))((a: A,el: MyEither[E,List[B]]) => map2(f(a),el, (aa,l) => aa::l))

  def sequence2[E,A](xs: List[MyEither[E,A]]): MyEither[E,List[A]] = traverse(xs,identity)

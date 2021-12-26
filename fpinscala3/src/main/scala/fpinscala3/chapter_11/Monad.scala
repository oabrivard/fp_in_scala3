package fpinscala3.chapter_11

import fpinscala3.chapter_5.MyStream
import fpinscala3.chapter_6.*
import fpinscala3.chapter_7.Nonblocking.Par
import fpinscala3.chapter_9.*

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla) {_::_})
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma)) // map(ma){a => List.fill(n)(a)}
  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]())) {(a,mla) => map2(f(a),mla) { (bool,xs) => if bool then a::xs else xs }}
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMapC[A,B](ma: F[A])(f: A => F[B]): F[B] = compose((_:Unit) => ma, f)(())
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
  def flatMapJ[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
  def composeJ[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}

object ListMonad extends Monad[List] {
  def unit[A](a: => A): List[A] = List(a)
  def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
}

object streamMonad extends Monad[MyStream] {
  def unit[A](a: => A): MyStream[A] = MyStream(a)
  def flatMap[A,B](ma: MyStream[A])(f: A => MyStream[B]): MyStream[B] = ma.flatMap(f)
}

object optionMonad extends Monad[Option] {
  def unit[A](a: => A): Option[A] = Some(a)
  def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
}

def parserMonad[PE,P[+_]](p: Parsers[PE,P]) = new Monad[P] {
  def unit[A](a: => A) = p.succeed(a)
  def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
}

val parMonad = new Monad[Par] {
  def unit[A](a: => A): Par[A] = Par.unit(a)
  def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = ma.flatMap(f)
}

def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
  def unit[A](a: => A): State[S, A] = State.unit(a)
  override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

val idMonad = new Monad[Id] {
  def unit[A](a: => A): Id[A] = Id(a)
  def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
}

val F = stateMonad[Int]

def zipWithIndex[A](as: List[A]): List[(Int,A)] =
  as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
    xs <- acc
    n  <- State.get
    _  <- State.set(n + 1)
  } yield (n, a) :: xs).run(0)._1.reverse

case class Reader[R, A](run: R => A)
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =  Reader(r => {
      val a = st.run(r)
      f(a).run(r)
    })
  }
}

val stringReaderM = Reader.readerMonad[String]
val intReader = Reader[String,Int](s => s.toInt)
val div2M = stringReaderM.flatMap(intReader)(i => stringReaderM.unit(i / 2.0))
val div2 = div2M.run("100")

package fpinscala3.chapter_12

import fpinscala3.chapter_11.Functor

import scala.runtime.Nothing$

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa,fab)((a,ab) => ab(a))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
    /*
    val fp = unit((a:A) => b => f(a,b))
    apply(apply(fp)(fa))(fb)
    */
  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    /*
    map2(fa, unit(()))((a, _) => f(a))
    */
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(map(fa)(f.curried))(fb))(fc)
  def map4[A,B,C,D,E](fa:
                      F[A],
                      fb:
                      F[B],
                      fc:
                      F[C],
                      fd:
                      F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def product[G[_]](g: Applicative[G]) : Applicative[({type f[x] = (F[x], G[x])})#f] =
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a),g.unit(a))
      override def apply[A,B](fabs: (F[A => B],G[A => B]))(a: (F[A],G[A])): (F[B],G[B]) =
        (self.apply(fabs._1)(a._1),g.apply(fabs._2)(a._2))
  }

  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit((g.unit(a)))
      override def map2[A,B,C](fga: F[G[A]],fgb: F[G[B]])(f: (A, B) => C) : F[G[C]] =
        self.map2(fga,fgb) {(ga,gb) => g.map2(ga,gb)(f)}
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  override def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))

  def compose[G[_]](g: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] =
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit((g.unit(a)))
      override def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        def toGB(fgb: F[G[B]]): G[B] = ??? // Impossible
        self.flatMap(fga){ga => self.unit(g.flatMap(ga) {a => toGB(f(a))}) }
    }
}

val streamApplicative = new Applicative[LazyList] {
  def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
  override def map2[A,B,C](a: LazyList[A], b: LazyList[B])(f: (A,B) => C): LazyList[C] =
    a zip b map f.tupled
}

def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
  def unit[A](a: => A): Either[E,A] = Right(a)
  override def flatMap[A,B](e: Either[E,A])(f: A => Either[E,B]): Either[E,B] = e flatMap f
}

enum Validation[+E, +A]:
  case Failure(head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case Success(a: A) extends Validation[Nothing, A]

def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
  def unit[A](a: => A): Validation[E,A] = Validation.Success(a)
  override def map2[A,B,C](va: Validation[E,A], vb: Validation[E,B])(f: (A,B) => C): Validation[E,C] =
    (va, vb) match {
      case (Validation.Success(a), Validation.Success(b)) => Validation.Success(f(a, b))
      case (Validation.Failure(h1, t1), Validation.Failure(h2, t2)) => Validation.Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (e@Validation.Failure(_, _), _) => e
      case (_, e@Validation.Failure(_, _)) => e
    }
}


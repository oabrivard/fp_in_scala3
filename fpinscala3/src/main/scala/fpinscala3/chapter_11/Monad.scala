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

package fpinscala3.chapter_10

import fpinscala3.chapter_8.*

trait Monoid[A]:
  def op(a1: A, a2: A): A
  def zero: A

val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]) = a1 ++ a2
  val zero = Nil
}

val intAddition: Monoid[Int] = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 + a2
  val zero = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 * a2
  val zero = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 || a2
  val zero = false
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 && a2
  val zero = true
}

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
  val zero = None
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(f1: A => A, f2: A => A) = f1 compose f2
  val zero = identity
}

def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}

def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
  Prop.forAll(gen) {a => m.op(a,m.zero)==a && m.op(m.zero,a)==a} &&
  Prop.forAll(gen ** gen ** gen) {as => m.op(m.op(as._1._1,as._1._2),as._2) == m.op(as._1._1,m.op(as._1._2,as._2))}
}

def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.zero)((b,a) => m.op(b,f(a)))
}

def foldRight[A,B](as: List[A])(z: B)(op: (A,B) => B): B = {
  foldMap(as,endoMonoid[B])(op.curried)(z)
}

def foldLeft[A,B](as: List[A])(z: B)(op: (B, A) => B): B = {
  foldMap(as,dual(endoMonoid[B]))(a => b => op(b,a))(z)
}

def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if (v.length == 0) {
    m.zero
  } else if (v.length == 1) {
    f(v.head)
  } else {
    val (left, right) = v.splitAt(v.length / 2)
    m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }
}


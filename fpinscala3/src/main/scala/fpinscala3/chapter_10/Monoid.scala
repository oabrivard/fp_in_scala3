package fpinscala3.chapter_10

import fpinscala3.chapter_7.Nonblocking.*
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

def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  def op(p1: Par[A], p2: Par[A]) : Par[A] = p1.map2(p2)(m.op)
  val zero = Par.unit(m.zero)
}

def parFoldMap2[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = Par.parMap(v)(f).flatMap { bs =>
  foldMapV(bs, par(m))(b => Par.lazyUnit(b))
}
/*
def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
  val pm = par(m)

  if (v.length == 0) {
    pm.zero
  } else if (v.length == 1) {
    Par.lazyUnit(f(v.head))
  } else {
    val (left, right) = v.splitAt(v.length / 2)
    pm.op(parFoldMap(left, m)(f), parFoldMap(right, m)(f))
  }
}
*/

val intComparison: Monoid[Int] = new Monoid[Int] {
  def op(a1: Int, a2: Int) = if a1 <= a2 then a2 else Int.MaxValue
  val zero = Int.MinValue
}

def isOrdered(v: IndexedSeq[Int]): Boolean = foldMap(v.toList, intComparison)(identity) < Int.MaxValue

enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  override def op(wc1: WC, wc2: WC): WC = (wc1,wc2) match {
    case (WC.Stub(s1),WC.Stub(s2)) => WC.Stub(s1+s2)
  }
  override def zero: WC = WC.Stub("")
}
package fpinscala3.chapter_10

import fpinscala3.chapter_7.Nonblocking.*
import fpinscala3.chapter_8.*
import fpinscala3.chapter_3.*

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
    case (WC.Part(ls1,wc1,rs1),WC.Part(ls2,wc2,rs2)) => WC.Part(ls1,wc1 + wc2 + (if (rs1+ls2).isEmpty then 0 else 1),rs2)
    case (WC.Part(ls1,wc1,rs1),WC.Stub(s2)) => WC.Part(ls1,wc1, rs1+s2)
    case (WC.Stub(s1),WC.Part(ls2,wc2,rs2)) => WC.Part(s1+ls2, wc2, rs2)
    case (WC.Stub(s1),WC.Stub(s2)) => WC.Stub(s1+s2)
  }
  override def zero: WC = WC.Stub("")
}

def countChars(s: String) : Int = {
  def charToWC(c: Char): WC = if c.isWhitespace then WC.Part("",0,"") else WC.Stub(c.toString)

  def unstub(s: String): Int = s.length min 1

  foldMapV(s.toIndexedSeq,wcMonoid)(charToWC) match {
    case WC.Stub(s) => unstub(s)
    case WC.Part(ls,wc,rs) => unstub(ls) + wc + unstub(rs)
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =  foldMap(as)(a => b => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
}

object StreamFoldable extends Foldable[LazyList] {
  override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
}

object MyTreeFoldable extends Foldable[MyTree] {
  override def foldMap[A,B](as: MyTree[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(f,mb.op)
}
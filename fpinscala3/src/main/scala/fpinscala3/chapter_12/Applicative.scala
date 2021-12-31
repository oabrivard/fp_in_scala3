package fpinscala3.chapter_12

import fpinscala3.chapter_10.{Foldable, Monoid}
import fpinscala3.chapter_11.Functor
import fpinscala3.chapter_6.State

import scala.runtime.Nothing$
import scala.language.implicitConversions

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

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map[K,V]())) { (kfv,fm) => map2(kfv._2,fm) {(v,m) => m + (kfv._1 -> v)} }
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

type Id[A] = A
def idApplicative = new Monad[Id] {
  def unit[A](a: => A): Id[A] = a
  override def flatMap[A,B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
}

type Const[M, B] = M

implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
  new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero
    override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
  }

def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
  def unit[A](a: => A): State[S, A] = State.unit(a)
  override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse(fa){a => idApplicative.unit(f(a))}(idApplicative)
  override def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)((a: A) => for {
    s1 <- State.get[S]
    (b, s2) = f(a, s1)
    _  <- State.set(s2)
  } yield b).run(s)

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B]) (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
    /*
    val l = traverse(fa)(f)(G)
    val r = traverse(fa)(g)(H)
    (l,r)
    */

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) : M[F[G[B]]] =
        self.traverse(fa)(ga => G.traverse(ga)(f))
    }

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
    mapAccum(as,z)((a,s) => ((),f(s,a)))._2

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] = (mapAccum(fa, toList(fb)) {
    case (a, Nil) => sys.error("zip: Incompatible shapes.")
    case (a, b :: bs) => ((a, b), bs) })._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] = (mapAccum(fa, toList(fb)) {
    case (a, Nil) => ((a, None), Nil)
    case (a, b :: bs) => ((a, Some(b)), bs) })._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = (mapAccum(fb, toList(fa)) {
    case (b, Nil) => ((None, b), Nil)
    case (b, a :: as) => ((Some(a), b), as) })._1
}

val optionTraverse = new Traverse[Option] {
  override def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f
}

val listTraverse = new Traverse[List] {
  override def map[A,B](fa: List[A])(f: A => B): List[B] = fa map f
}

case class Tree[+A](head: A, tail: List[Tree[A]])

val treeTraverse = new Traverse[Tree] {
  override def map[A,B](fa: Tree[A])(f: A => B): Tree[B] = Tree(f(fa.head), fa.tail.map(t => map(t)(f)))
}


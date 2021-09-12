package fpinscala3.chapter_8

import fpinscala3.chapter_6.*
import fpinscala3.chapter_6.RNG.{flatMap, int, nonNegativeInt, sequenceWithFold, unit}
import fpinscala3.chapter_7.Par
import fpinscala3.chapter_8.Gen.*
import fpinscala3.chapter_8.Prop.*

import java.util.concurrent.{ExecutorService, Executors}

/*
  sum: List[Int] => Int
    val intList1 = Gen.listOf(Gen.choose(0,100))
    forAll(intList1)(ns => sum(ns.reverse) == sum(ns))
    forAll(intList1)(ns => sum(ns.drop(1)) == sum(ns) - nh.head)
    forAll(intList1)(ns => sum(0::ns) == sum(ns))
    val intList2 = List of n elements of value m
    forAll(intList2)(ns => sum(ns) == n*m)
    val intList3 = List of n consecutive elements of value ranging from a to b
    forAll(intList3)(ns => sum(ns) == (n+1)(a+b)/2)

  max: List[Int] => Int
    val intList1 = Gen.listOf(Gen.choose(0,100))
    forAll(intList1)(ns => ns.sorted.reverse.head == max(ns))
    val intList2 = List of n elements of value m
    forAll(intList2)(ns => max(ns) == m)
    val intList3 = List of n consecutive elements of value ranging from a to b
    forAll(intList3)(ns => max(ns) == b)
*/

enum Result(val isFalsified: Boolean):
  case Falsified(failure: FailedCase, successes: SuccessCount) extends Result(true)
  case Passed extends Result(false)
  case Proved extends Result(false)

case class Prop(run: (MaxSize,TestCases,RNG) => Result):
  def &&(p: Prop): Prop = Prop {(max,n,rng) => {
    val r = run(max,n,rng)
    if r.isFalsified then r else p.run(max,n,rng)
  }}

  def ||(p: Prop): Prop = Prop {(max,n,rng) => { run(max,n,rng) match
    case Result.Falsified(failure,_) => p.tag(failure).run(max,n,rng)
    case p => p
  }}

  def tag(msg: FailedCase): Prop = Prop {(max,n,rng) => { run(max,n,rng) match
    case Result.Falsified(failure,successes) => Result.Falsified(msg + "\n" +failure,successes)
    case p => p
  }}

object Prop:
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Result.Passed else Result.Falsified(a.toString, i)
        } catch {
          case e: Exception => Result.Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Result.Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match
      case Result.Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Result.Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Result.Proved =>
        println(s"+ OK, proved property.")

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Result.Passed else Result.Falsified("()", 0)
  }

  val ES: ExecutorService = Executors.newCachedThreadPool

  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val p3 = check {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  val p4 = checkPar {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0,10) map (Par.unit(_))
  val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val p6 = forAllPar(pint)(n => equal(Par.fork(n), n))


object Gen:
  /*
  Implement Gen.choose using this representation of Gen. It should generate integers in the
  range start to stopExclusive. Feel free to use functions you’ve already written.
  */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State {
      RNG.map(RNG.nonNegativeInt) { n => start + n % (stopExclusive - start) }
    })

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = choose(0,2) map {(n:Int) => if n > 0 then true else false}

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    val l = List.fill(n)(g.sample)
    Gen(State.sequence(l))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {listOfN(_,g)}

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))

  object **:
    def unapply[A,B](p: (A,B)) = Some(p)

/*
trait Gen[A]:
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
*/
case class Gen[+A](sample: State[RNG,A]):
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap {Gen.listOfN(_,this)}

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap {if (_) then g1 else g2}

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    val ratio = g1._2.abs / (g1._2.abs+g2._2.abs) * 100.0
    choose(0,101) flatMap { (i:Int) => if (i > ratio.toInt) g2._1 else g1._1 }

  def unsized: SGen[A] = SGen { _ => this}

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))

case class SGen[+A](forSize: Int => Gen[A]):
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i => forSize(i).flatMap(f(_)(i)) }
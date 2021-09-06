package fpinscala3.chapter_8

import fpinscala3.chapter_6.*
import fpinscala3.chapter_6.RNG.{flatMap, int, nonNegativeInt, sequenceWithFold, unit}
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

trait Prop:
  def check: Boolean
  def &&(p: Prop): Prop = new Prop:
    def check = Prop.this.check && p.check

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

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

/*
trait Gen[A]:
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
*/
case class Gen[A](sample: State[RNG,A]):
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]

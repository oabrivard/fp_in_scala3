package fpinscala3.chapter_8

import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

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
  def unit[A](a: => A): Gen[A] = ???

trait Gen[A]:
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]

@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

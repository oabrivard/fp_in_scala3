package fpinscala3.chapter_9

import scala.util.matching.Regex
import scala.language.implicitConversions
import fpinscala3.chapter_8._

trait Parsers[ParseError, Parser[+_]]:
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] = string(c.toString) map {_.charAt(0)}
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if n==0 then succeed(List[A]()) else map2(p,listOfN(n-1,p))(_::_)
  def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p,many(p))(_::_),succeed(List[A]()))
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(a2 => succeed(f(a2)))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p,many(p)) {_::_}
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {a <- p ; b <- p2} yield (a,b)
    //flatMap(p) {a => flatMap(p2) {b => succeed((a,b))}}
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = product(p,p2).map((x) => f(x._1,x._2))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]):
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)
    def slice = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  val numA: Parser[Int] = char('a').many.map(_.size)
  val numAv2: Parser[Int] = char('a').many.slice.map(_.size)
  val ctxSens: Parser[List[Char]] = regex("[0-9]".r).flatMap(s => listOfN(s.toInt, char('a')))

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
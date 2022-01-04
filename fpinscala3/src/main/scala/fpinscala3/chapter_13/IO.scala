package fpinscala3.chapter_13

import fpinscala3.chapter_12.Monad

import scala.io.StdIn.readLine

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
    /*
      can't just be f(self.run).
      Otherwise f would be executed during expression construction (IO construction)
      and not expression execution (IO.run)
     */
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  override def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}

def ReadLine: IO[String] =  IO { readLine }

def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

def converter: IO[Unit] = for {
  _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
  d <- ReadLine.map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()

def converter2: IO[Unit] =
  IO.unit(println("Enter a temperature in degrees Fahrenheit: "))
    .flatMap(_ => IO.unit("180").map(_.toDouble)
      .flatMap(d => IO.unit(println(fahrenheitToCelsius(d).toString)))
        .map(_ => ()))

def converter3: IO[Unit] =
  new IO[Unit] {
    def run = println("Enter a temperature in degrees Fahrenheit: ")
  }.flatMap(_ =>
    new IO[String] {
      def run = "180"
    }.map(_.toDouble).flatMap(d =>
      new IO[Unit] {
        def run = println(fahrenheitToCelsius(d).toString)
      }
    ).map(_ => ())
  )

def test: IO[Unit] =
  new IO[Unit] {
    def run = println("A")
  }.flatMap(_ =>
      new IO[Unit] {
        def run = println("B")
      }.map(_ => ())
    )


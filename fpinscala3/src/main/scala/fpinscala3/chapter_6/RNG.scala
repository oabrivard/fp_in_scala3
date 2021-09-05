package fpinscala3.chapter_6

trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

type Rand[+A] = RNG => (A, RNG)

object RNG:
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
    /*
    rng.nextInt match
      case (Int.MinValue,nextRng) => nonNegativeInt(nextRng)
      case (i,nextRng) if i < 0 => (-i,nextRng)
      case (i,nextRng) => (i,nextRng)
    */

  def double(rng: RNG): (Double, RNG) =
    val (i, r) = rng.nextInt
    (i.toDouble / (Int.MaxValue.toDouble + 1.0), r)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d),r2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i,d), r) = intDouble(rng)
    ((d,i),r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3),r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def loop(l: List[Int], r: RNG, i: Int): (List[Int], RNG) =
      if i==0 then
        (l,r)
      else {
        val (i, nextRng) = r.nextInt
        loop(i::l, nextRng, i-1)
      }

    loop(List(),rng,count)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def doubleWithMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1.0))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b),rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt // shortcup for rng => rng.nextInt

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match
    case h::t => map2(h,sequence(t)){ (x,xs) => x::xs }
    case List() => unit(List())

  def sequenceWithFold[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) {(ra,rla) => map2(ra,rla) {_::_}}

  def intsWithSequence(count: Int): Rand[List[Int]] = sequenceWithFold(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {i => {
      val mod = i % n
      if i + (n-1) - mod >= 0 then
        unit(mod)
      else
        nonNegativeLessThan(n)
    }}

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a,b)
      }
    }

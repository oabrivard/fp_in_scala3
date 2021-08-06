trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

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

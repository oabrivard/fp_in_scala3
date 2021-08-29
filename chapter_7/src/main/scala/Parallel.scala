import java.util.concurrent.TimeUnit

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

type Par[A] = ExecutorService => Future[A]

object Par:
  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  private case class Map2Future[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C]:
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = af.isCancelled || bf.isCancelled
    def cancel(evenIfRunning: Boolean) = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutMs: Long): C = cache match
      case Some(c) => c
      case None => {
        val start = System.currentTimeMillis
        val ar = af.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis; val at = stop-start
        val br = bf.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
      }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    e => Map2Future(a(e), b(e), f)
    //e => UnitFuture(f(a(e).get,b(e).get))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](e: ExecutorService)(a: Par[A]): Future[A] = a(e)

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def sortPar2(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil:List[A]))(map2(_,_)((a,l) => a::l))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]]

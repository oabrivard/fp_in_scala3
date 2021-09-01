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

  def sequentialExecutor[A]() : ExecutorService = new ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = UnitFuture(a.call)
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

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else List()))
    val seq: Par[List[List[A]]] = sequence(fas)
    map(seq)(l => l.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)

  def reduce[A](seq: IndexedSeq[A])(z: A)(f: (A,A) => A): Par[A] =
    if (seq.length <= 1)
      Par.unit(seq.headOption getOrElse z)
    else
      val (l,r) = seq.splitAt(seq.length/2)
      Par.map2(Par.fork(reduce(l)(z)(f)), Par.fork(reduce(r)(z)(f)))(f)

  def max(ints: IndexedSeq[Int]): Par[Int] = reduce(ints)(Int.MinValue)(_.max(_))

  def sumChar(lp: List[String]): Par[Int] = fork {
    val lw = lp.map(p => p.split(' ').toList).flatten
    val par = parMap(lw)(w => w.length)
    map(par)(l => l.sum)
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] =
    map2( map2(pa,pb) { (a,b) => (c:C) => f(a,b,c)}, pc) {(f,c) => f(c)}

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => run(es)(choices(run(es)(n).get))

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(Par.map(cond)(b => if b then 1 else 0))(List(t,f))

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es){choices(run(es)(pa).get)}

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond){if _ then t else f}

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n){choices(_)}

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val p = run(es)(a).get
    run(es)(p)
  }

  def flatMapViaJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join( map(pa)(f) )

  def joinViaFlatmap[A](a: Par[Par[A]]): Par[A] =
    chooser(a)(pa => pa)
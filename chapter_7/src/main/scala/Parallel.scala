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

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = e => UnitFuture(f(a(e).get,b(e).get))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](e: ExecutorService)(a: Par[A]): Future[A] = a(e)


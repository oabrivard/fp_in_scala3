package fpinscala3.chapter_2

def fib(n:Int) =
  @annotation.tailrec
  def loop(a: Int, b: Int, i: Int): Int = if i==n then a else loop(b, a+b, i+1)
  loop(0, 1, 0)

def isSorted[A](aa: List[A], order: (A, A) => Boolean) =
  @annotation.tailrec
  def loop(x: A, xs: List[A], isSorted: Boolean) : Boolean =
    if xs.isEmpty || !isSorted then
      isSorted
    else
      loop(xs.head, xs.tail, isSorted && order(x, xs.head))

  if aa.size < 2 then true else loop(aa.head, aa.tail, true)

def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a,b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

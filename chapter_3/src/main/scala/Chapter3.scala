enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  def of[A](aa: A*): MyList[A] =
    val tail = aa.drop(1)
    if aa.isEmpty then Nil else Cons(aa.head, of(tail:_*))

  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: MyList[Double]): Double = {
    def loop(l: MyList[Double]): Double =
      l match
        case Nil => 1.0
        case Cons (x, xs) => if (x == 0.0) 0.0 else x * loop(xs)

    if doubles==Nil then 0.0 else loop(doubles)
  }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))

  extension [A](l: MyList[A])
    def tail = l match
      case Nil => Nil
      case Cons(x, xs) => xs

    def setHead(x: A) = Cons(x, l)

    def drop(n: Int) = {
      @annotation.tailrec
      def loop(xs: MyList[A], i: Int): MyList[A] = if (i==0) xs else loop(xs.tail, i-1)

      loop(l, n)
    }

    def dropWhile(f: A => Boolean) = {
      @annotation.tailrec
      def loop(xs: MyList[A]): MyList[A] = xs match
        case Nil => Nil
        case Cons(x, xss) => if (!f(x)) xs else loop(xss)

      loop(l)
    }


    def init() = {
      @annotation.tailrec
      def loop(xs: MyList[A], acc: MyList[A]) : MyList[A] = xs match
        case Nil => Nil
        case Cons(x, Nil) => acc
        case Cons(x, xss) => loop(xss, MyList.append(acc, MyList.of(x)))

      loop(l, Nil)
    }

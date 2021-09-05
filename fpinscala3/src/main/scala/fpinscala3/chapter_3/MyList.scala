package fpinscala3.chapter_3

import javax.swing.plaf.multi.MultiListUI

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

  def foldRight[A,B](l: MyList[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z, f))

  @annotation.tailrec
  def foldLeft[A,B](l: MyList[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x), f)
  // wrong, does not fold right but left...
  // def foldRightWithFoldLeft[A,B](l: MyList[A], z: B, f: (A, B) => B): B =
  //  foldLeft(l, (x:B)=>x, (g:B=>B,x:A) => (y:B) => f(x,g(y)))(z)
  // (1,2,3)  0   f=x+y
  // Nil  frl=(z)->z    frl(0)=0
  // (1)  frl = h((z)->z,1) = (z)->f(1,(z)->z)  frl(0) = f(1,0)
  // (2)  frl = h((z)->f(1,(z)->z),2) = (z)->f(2,(z)->f(1,(z)->z))  frl(0) = f(2,f(1,0))

  def foldRightWithFoldLeft[A,B](l: MyList[A], z: B, f: (A, B) => B): B =
    foldLeft(l, (x:B)=>x, (g:B=>B,x:A) => (y:B) => g(f(x,y)))(z)
  // (1,2,3)  0   f=x+y
  // Nil  frl=(z)->z    frl(0)=0
  // (1) frl=(z)->((k)->k)(f(1,z))  frl(0)=((k)->k)(f(1,0)) = f(1,0)
  // (1,2) frl=(l)->((z)->((k)->k)(f(1,z)))(f(2,l))   frl(0) = ((z)->((k)->k)(f(1,z)))(f(2,0)) = ((k)->k)(f(1,f(2,0)))) = f(1,f(2,0))

  def foldLeftWithFoldRight[A,B](l: MyList[A], z: B, f: (B,A) => B): B =
    foldRight(l, (b:B) => b, (a:A,g) => y => g(f(y,a)))(z)
  // Nil,"_"   flr=(x0)->x0   flr("_")="_"
  // ('a'),"_"  flr = h('a',foldRight(Nil,(x0)->x0,h)) = h('a',(x0)->x0) = (x1) -> ((x0)->x0)(f(x1,'a'))
  //            flr("_") = h('a',(x0)->x0)("_") = ((x0)->x0)(f("_",'a')) = f("_",'a')
  //            h(a:A,b:B=>B) <=>  (z:B) => b(f(z,a))
  // ('a','b'),"_"  flr = h('a', foldRight('b',(x0)->x0,h))
  //                    = h('a',h('b',foldRight(Nil,(x0)->x0,h)))
  //                    = h('a',h('b',(x0)->x0)))
  //                    = h('a', (x1) -> ((x0)->x0)(f(x1,'b')) )
  //                    = (x2) -> ((x1) -> ((x0)->x0)(f(x1,'b')) ))(f(x2,'a'))
  //              flr("_") = ((x1) -> ((x0)->x0)(f(x1,'b')) ))(f("_",'a')) = ((x1) -> ((x0)->x0)(f(x1,'b')) ))("_a")
  //              flr("_") = ((x0)->x0)(f("_a",'b'))
  //              flr("_") = "_ab"

  def lengthWithFoldLeft[A](l: MyList[A]) = foldLeft(l, 0, (x,_) => x+1)

  def sumWithFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0, (x,y) => x+y)

  def productWithFoldLeft(doubles: MyList[Double]): Double = if doubles==Nil then 0.0 else foldLeft(doubles, 1.0, (x,y) => x*y)

  def appendWithFold[A](l1: MyList[A], l2: MyList[A]): MyList[A] = foldRight(l1,l2,(x,acc) => Cons(x,acc) )
  
  def concatenate[A](ll: MyList[MyList[A]]) : MyList[A] =
    foldRight(ll,MyList.Nil, (l:MyList[A],acc:MyList[A]) => append(l,acc))

  def transformInts(myList: MyList[Int]) : MyList[Int] = myList match
    case Nil => Nil
    case Cons(x,xs) => Cons(x+1, transformInts(xs))

  def transformDouble(myList: MyList[Double]) : MyList[String] = myList match
    case Nil => Nil
    case Cons(x,xs) => Cons(x.toString(), transformDouble(xs))

  def map[A,B](xs:MyList[A],f: (A)=>B): MyList[B] = xs match
    case Nil => Nil
    case Cons(x,s) => Cons(f(x), map(s, f))

  def map_2[A,B](xs:MyList[A],f: (A)=>B): MyList[B] =
    foldRight(xs, Nil, (x:A, s:MyList[B]) => Cons(f(x),s))

  def filter[A](xs:MyList[A],f: (A)=>Boolean): MyList[A] =
    foldRight(xs, Nil, (x:A, s:MyList[A]) => if f(x) then Cons((x),s) else s)

  def flatMap[A,B](xs:MyList[A],f: (A)=>MyList[B]): MyList[B] =
    foldRight(xs,MyList.Nil, (x:A,s:MyList[B]) => append(f(x),s))

  def filterWithFlatMap[A](xs:MyList[A],f: (A)=>Boolean): MyList[A] =
    flatMap(xs, x => if f(x) then Cons(x,Nil) else Nil)

  def addListOfInts(xs1:MyList[Int],xs2:MyList[Int]): MyList[Int] = xs1 match
    case Nil => Nil
    case Cons(x1,s1) => xs2 match
      case Nil => Nil
      case Cons(x2,s2) => Cons(x1+x2, addListOfInts(s1,s2))

  def zipWith[A,B](xs1:MyList[A],xs2:MyList[A], f: (A,A)=>B): MyList[B] = xs1 match
    case Nil => Nil
    case Cons(x1,s1) => xs2 match
      case Nil => Nil
      case Cons(x2,s2) => Cons(f(x1,x2), zipWith(s1,s2,f))

  def length[A](xs:MyList[A]) = MyList.foldRight(xs, 0, (_,acc) => acc+1)

  def startWith[A](xs1:MyList[A],xs2:MyList[A]): Boolean =
    val xs = zipWith(xs1,xs2, _==_)
    length(xs1)>=length(xs2) && foldLeft(xs,true, _&&_)

  @annotation.tailrec
  def hasSubsequence[A](xs1:MyList[A],xs2:MyList[A]): Boolean = xs1 match
    case Nil => false
    case Cons(x,s) => startWith(xs1,xs2) || hasSubsequence(s,xs2)

extension [A](l: MyList[A])
  def tail = l match
    case MyList.Nil => MyList.Nil
    case MyList.Cons(x, xs) => xs

  def setHead(x: A) = MyList.Cons(x, l)

  def drop(n: Int) = {
    @annotation.tailrec
    def loop(xs: MyList[A], i: Int): MyList[A] = if (i==0) xs else loop(xs.tail, i-1)

    loop(l, n)
  }

  def dropWhile(f: A => Boolean) = {
    @annotation.tailrec
    def loop(xs: MyList[A]): MyList[A] = xs match
      case MyList.Nil => MyList.Nil
      case MyList.Cons(x, xss) => if (!f(x)) xs else loop(xss)

    loop(l)
  }

  def init() = {
    @annotation.tailrec
    def loop(xs: MyList[A], acc: MyList[A]) : MyList[A] = xs match
      case MyList.Nil => MyList.Nil
      case MyList.Cons(x, MyList.Nil) => acc
      case MyList.Cons(x, xss) => loop(xss, MyList.append(acc, MyList.of(x)))

    loop(l, MyList.Nil)
  }

  def length = MyList.length(l)

  def reverse() = MyList.foldLeft(l, MyList.Nil, (x:MyList[A],y:A) => MyList.Cons(y,x))


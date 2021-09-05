package fpinscala3.chapter_3

enum MyTree[+A]:
  case Leaf(value: A)
  case Node(left: MyTree[A], right: MyTree[A])

object MyTree:
  def maximum(t: MyTree[Int]): Int = t match
    case MyTree.Leaf(value) => value
    case MyTree.Node(left,right) => math.max(maximum(left), maximum(right))

extension [A](t: MyTree[A])
  def size: Int = t match
    case MyTree.Leaf(_) => 1
    case MyTree.Node(left,right) => left.size + right.size

  def depth: Int = t match
    case MyTree.Leaf(_) => 0
    case MyTree.Node(left,right) => math.max(left.depth, right.depth) + 1

  def map[B](f:A=>B): MyTree[B] = t match
    case MyTree.Leaf(value) => MyTree.Leaf(f(value))
    case MyTree.Node(left,right) => MyTree.Node(left.map(f),right.map(f))

  def fold[B,C](f: A=>B, g: (B,B)=>B): B = t match
    case MyTree.Leaf(value) => f(value)
    case MyTree.Node(left,right) => g(left.fold(f,g), right.fold(f,g))

  def mapWithFold[B](f:A=>B): MyTree[B] =
    t.fold(value=>MyTree.Leaf(f(value)),(l,r) => MyTree.Node(l,r))

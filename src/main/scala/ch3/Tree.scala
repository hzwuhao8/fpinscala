package ch3

trait Tree[+A] {

}

case class Leaf[A](v: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {

    case Leaf(a)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(a)      => a
    case Branch(l, r) => Math.max(max(l), max(r))
  }
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(a)      => 0
    case Branch(l, r) => 1 + Math.max(depth(l), depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}

object Exam3 {
  def main(args: Array[String]): Unit = {
    val t1 = Leaf(1)
    val t2 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))

    println(t2)
    println(s"t2.size= ${Tree.size(t2)}")

    val t3 = Tree.map(t2)(_.head.toInt)
    println(t3)
    println(s"t3.max= ${Tree.max(t3)}")
    println(s"t3.depth= ${Tree.depth(t3)}")
  }

}
package ch3

sealed trait List[+A] {

}
case object Nil extends List[Nothing] {

}
case class Cons[+A](head: A, t: List[A]) extends List[A] {

}

object List {

  def empty[A](): List[A] = Nil
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil         => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => throw new RuntimeException()
    case Cons(h, t) => t
  }
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil        => throw new RuntimeException()
    case Cons(h, t) => Cons(a, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)                  => Nil
    case (Cons(h, t), x) if x <= 0 => l
    case (Cons(h, t), x) if x == 1 => t
    case (Cons(h, t), x)           => drop(t, x - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil                  => Nil
    case Cons(h, t) if (f(h)) => dropWhile(t)(f)
    case _                    => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil         => z
      case Cons(h, xs) => f(h, foldRight(xs, z)(f))
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(h, xs) => foldLeft(xs, f(z, h))(f)
  }

  def sum1(l: List[Int]) = foldRight(l, 0)(_ + _)
  def sum2(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def mkString[A](l: List[A]): String = foldLeft(l, "")((str, a) => a.toString + "," + str)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, empty[A]())((ll: List[A], a: A) => Cons(a, ll))
  }

  def append1[A](a: A, l: List[A]): List[A] = {
    reverse(Cons(a, reverse(l)))
  }

  def append2[A](a: A, l: List[A]): List[A] = {

    foldLeft(reverse(l), List(a))((ll: List[A], a: A) => Cons(a, ll))
  }

  def append3[A](a: A, l: List[A]): List[A] = {
    foldRight(l, List(a))((a: A, ll: List[A]) => Cons(a, ll))
  }

  def map1[A, B](l: List[A])(f: A => B): List[B] = {
    val as = foldLeft(l, empty[B])((as: List[B], a: A) => Cons(f(a), as))
    reverse(as)
  }

  def map2[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil          => Nil
    case Cons(a, Nil) => Cons(f(a), Nil)
    case Cons(a, t)   => Cons(f(a), map2(t)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil        => Nil
    case Cons(a, t) => if (f(a)) Cons(a, filter(t)(f)) else filter(t)(f)
  }
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    val as = foldLeft(l, empty[A])((ll: List[A], a: A) => if (f(a)) Cons(a, ll) else ll)
    reverse(as)
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val as = reverse(l)
    foldLeft(as, z)((b, a) => f(a, b))
  }

  def :::[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil        => l2
      case Cons(a, t) => Cons(a, :::(t, l2))
    }
  }
  def flatten[A](l1: List[List[A]]): List[A] = l1 match {
    case Nil          => Nil
    case Cons(a, Nil) => a
    case Cons(a, t)   => :::(a, flatten(t))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil          => Nil
    case Cons(a, Nil) => f(a)
    case Cons(a, t) =>
      val tmp = f(a)
      val tmp2 = flatMap(t)(f)
      :::(tmp, tmp2)
  }
  def filter3[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((a) => if (f(a)) List(a) else Nil)
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, Nil)                 => Nil
      case (_, Nil)                   => Nil
      case (Nil, _)                   => Nil
      case (Cons(a, t1), Cons(b, t2)) => Cons(f(a, b), zipWith(t1, t2)(f))
    }
  }
}

object Exam {
  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3 = List("a", "b")
    val l = List(1, 2, 3, 4, 5)
    val x = l match {
      case Cons(x, Cons(2, Cons(4, _)))           => x
      case Nil                                    => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, __)))) => x + y
      case Cons(h, t)                             => h + List.sum(t)
      case _                                      => 101
    }
    println(s"x == ${x}, '3'  ")
    println(s"${List.tail(l)}")
    println(s"${List.setHead(l, 100)}")
    println(s"${List.drop(l, 3)}")
    println(s"${List.dropWhile(l)(x => x < 3)}")
    println(s"${List.init(l)}")
    val list = List((1 to 7).map { x => scala.util.Random.nextInt(1000) }: _*)
    println(s"list=${list}")
    println(s"sum1(l)=${List.sum1(list)}")
    println(s"sum2(l)=${List.sum2(list)}")

    print("reverse\t")
    println(List.reverse(list))
    print("mkString\t")
    println(List.mkString(list))
    print("append1\t")
    println(List.append1(10, list))
    print("append2\t")
    println(List.append2(10, list))
    print("append3\t")
    println(List.append3(10, list))
    print("map1\t")
    println(List.map1(list)(x => x))

    print("map2\t")
    println(List.map2(list)(x => x))
    print("filter\t")
    println(List.filter(list)(x => x > 500))

    print("filter2\t")
    println(List.filter2(list)(x => x > 500))

    print("filter3 use flatMap\t")
    println(List.filter3(list)(x => x > 500))

    print("flatMap\t")
    println(List.flatMap(list)(x => List(x, x)))

    print("zipWith List(1,2,3), List(4,5,6) with + \n")
    println(List.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b))
    println(List.zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => (a, b)))
  }

}
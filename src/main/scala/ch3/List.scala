package ch3

sealed trait List[+A] {

}
case object Nil extends List[Nothing] {

}
case class Cons[+A](head: A, t: List[A]) extends List[A] {

}

object List {
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

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(h, xs) => f(foldLeft(xs, z)(f), h)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B, r: B)(f: (B, A) => B): B = as match {
    case Nil         => r
    case Cons(h, xs) => foldLeft(xs, z, f(r, h))(f)
  }

  def sum1(l: List[Int]) = foldRight(l, 0)(_ + _)
  def sum2(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def sum3(l: List[Int]) = foldLeft(l, 0, 0)(_ + _)
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
    val list = List( (1 to 10).map{ x=> scala.util.Random.nextInt(1000)}: _*)
    println(s"list=${list}")
    println(s"sum1(l)=${List.sum1(list)}")
    println(s"sum2(l)=${List.sum2(list)}")
    println(s"sum3(l)=${List.sum3(list)}")
    
  }

}
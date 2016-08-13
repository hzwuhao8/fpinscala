package ch5

import scala.collection.mutable.Buffer

sealed trait Stream[+A] {

  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) =>
      val a = h()
      val tt = t()
      a :: (tt.toList())
  }

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def take2(s: Stream[A], n: Int, r: Buffer[A]): Buffer[A] = s match {
      case Empty                  => r
      case Cons(h, t) if (n <= 0) => r
      case Cons(h, t) if (n > 0) =>
        r.append(h())
        take2(t(), n - 1, r)
    }
    val buf = take2(this, n, Buffer.empty)
    Stream(buf: _*)
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty                  => Empty
    case Cons(h, t) if (n > 0)  => t().drop(n - 1)
    case Cons(h, t) if (n <= 0) => t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def take2(s: Stream[A], r: () => Stream[A]): Stream[A] = s match {
      case Empty                   => r()
      case Cons(h, t) if (!p(h())) => r()
      case Cons(h, t) if (p(h()))  => take2(t(), () => Cons(h, r))
    }
    take2(this, () => Stream.empty[A])
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  /**
   * 自动 提前终止
   */
  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) { (a, b) => if (f(a)) Stream.cons(a, b) else b }
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s) { (a, b) => Stream.cons(a, b) }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h) append t)
  }

  def toList2(): List[A] = {
    foldRight(Nil: List[A])((h, t) => h :: t)
  }

  def otherMap[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }
  }

  def otherTake(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), 1)          => Some((h(), (Stream.empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }
  }

  def otherTakeWhile(f: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case (Cons(h, t)) if f(h()) => Some((h(), (t())))
      case _                      => None
    }
  }
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, s2)) {
      case ((Cons(h1, t1), Cons(h2, t2))) => Some((f(h1(), h2()), (t1(), t2())))
      case _                              => None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case ((Cons(h1, t1), Cons(h2, t2))) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case ((Cons(h1, t1), Empty))        => Some(((Some(h1()), None), (t1(), Stream.empty)))
      case ((Empty, Cons(h2, t2)))        => Some(((None, Some(h2())), (Empty, t2())))
      case _                              => None
    }
  }

  def startWith[B >: A](ss: Stream[B]): Boolean = {
    this.zipAll(ss).takeWhile( !_._2.isEmpty).forAll(x => x._1 == x._2)
  }
  
  def tails(): Stream[Stream[A]] = {
    Stream.unfold(this){
      case Empty => None
      case Cons(h,t) => Some((Cons(h,t),t() ))
    } append Stream(Stream.empty)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   *  s = a0
   * Some(a1, s) = f(s)
   * Some(a2, s) = f(s)
   * s 下一个状态
   */

  def ones: Stream[Int] = cons(1, ones)
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a + b))

  def seq2(n: Int): Stream[Int] = cons(n * n, seq2(n + 1))

  def seqx[B](n: Int)(f: Int => B): Stream[B] = cons(f(n), seqx(n + 1)(f))

  def seq3(n: Int) = seqx(n)(x => x * x * x)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case _            => Stream.empty[A]
    }
  }

  /**
   * 常数序列
   */
  def ones2(n: Int): Stream[Int] = unfold(n)(x => Some((n, x + 1)))
  /**
   * 等差1 的序列
   */
  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))
  /**
   * 等差d的序列
   * a(n) = a(n-1)+d=a(n-2)+ 2d = a(n-x) + x*d = a0 + n*d
   * x 是 an  吗？
   * Some((a,s))
   * a 是 an
   * s 是下一次计算的参数
   */
  /**
   * 这个 实际 每次是 从 a0 计算得到结果
   */
  def from3a(n: Int, d: Double): Stream[Double] = unfold(n)(x => Some(n + (x * d), x + 1))
  /**
   * 从 a(n-1) 计算得到 a(n)
   */
  def from3b(n: Double, d: Double): Stream[Double] = unfold(n) { x =>
    val an = x + d
    val s = an
    Some((an, s))
  }
  /**
   * a(n) = n * d = a(n-1) + d
   * 0,d,1*d,2*d,3*d
   */
  def from4(d: Int): Stream[Int] = unfold(0)(x => Some(x * d, x + 1))
  def from4b(d: Int): Stream[Int] = unfold(0) { x =>
    val an = x
    val z = an + d
    Some(an, z)
  }
  /**
   * an = d^n  等比
   */
  def from5(n: Int, d: Int): Stream[Int] = unfold(n)(x => Some(x, x * d))

  /**
   * an = n^d
   */
  def from6(n: Int, d: Int): Stream[Int] = unfold(n)(x => Some(List.fill(d)(x).product, x + 1))

  /**
   * 计算 a(n) 需要知道 a(n-1),a(n-2)
   * 所以 参数是(Int,Int)
   */
  def fibs2(): Stream[(Int)] = unfold((0, 1)) {
    case (f0, f1) =>
      val an = f0
      val z = (f1, f0 + f1)
      Some((an, z))
  }
}

object Exam {

  import Stream._

  def main(args: Array[String]): Unit = {
    val s1 = Stream(1, 2, 3, 4, 5, 6)
    val t1 = s1.toList
    println(t1)
    println(s1.take(1).toList)
    println(s1.take(2).toList)
    println(s1.takeWhile(_ < 4).toList)
    println(s1.takeWhile2(_ < 3).toList)
    println(s"exists _ < 3 = ${s1.exists(_ < 3)}")
    println(s"forAll _ < 3 = ${s1.forAll(_ < 3)}")
    println(s"filter _ < 3 = ${s1.filter(_ < 3).toList}")
    println(s"map _ +1 = ${s1.map(_ + 1).toList}")
    println(s"append  Stream(100) = ${s1.append(Stream(100)).toList2}")
    println("drop")
    println(s1.drop(1).toList)
    println(s1.drop(2).toList)

    println(s"ones.take(5)  ${ones.take(5).toList}")
    println(s"ones2.take(5)  ${ones2(2).take(5).toList}")

    println(s"from.take(5)  ${from(1).take(5).toList}")
    println(s"n from2(1).take(5)  ${from2(1).take(5).toList}")
    println(s"n+d from3a(1, 0.5).take(5)  ${from3a(1, 0.5).take(5).toList}")
    println(s"n+d from3b(1, 0.5).take(5)  ${from3b(1, 0.5).take(5).toList}")
    println(s"d*n from4(20).take(9)  ${from4(20).take(9).toList}")
    println(s"d*n from4b(20).take(9)  ${from4b(20).take(9).toList}")
    println(s"3^n from5(1, 3).take(9)  ${from5(1, 3).take(9).toList}")
    println(s"n^2 from6(1, 2).take(9)  ${from6(1, 2).take(9).toList}")

    println(s"fibs.take(10)  ${fibs(0, 1).take(10).toList}")
    println(s"fibs2.take(10)  ${fibs2().take(10).toList}")
    println(s"seq3.take(10)  ${seq3(1).take(10).toList}")

    println(s"sum( 1/(n*n)) .take(10)  ${seqx(1)(x => 1.toDouble / (x * x)).take(20).toList.sum / Math.PI}")
    val sa1 = Stream(1,2,3)
    val sa2 = Stream(1,2)
    val sa3 = Stream(1,2,3,4)
    println(s"${sa1.toList} startWith ${sa2.toList} =  ${sa1.startWith(sa2)}")
    println(s"${sa1.toList} startWith ${sa3.toList} =  ${sa1.startWith(sa3)}")
    val sb = sa3.tails()
    sb.otherMap { x => println( x.toList) }.toList
  }
}
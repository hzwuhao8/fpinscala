package ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val intAddition: Monoid[Int] = new Monoid[Int] {
    val zero = 0
    def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    val zero = 1
    def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    val zero = false
    def op(a: Boolean, b: Boolean) = a || b
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    val zero = true
    def op(a: Boolean, b: Boolean) = a && b
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    val zero = None
    def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    val zero = (ma.zero, mb.zero)
    def op(x: (A, B), y: (A, B)): (A, B) = {
      val a1 = x._1
      val b1 = x._2
      val a2 = y._1
      val b2 = y._2
      (ma.op(a1, a2), mb.op(b1, b2))
    }
  }

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    val zero = (a: A) => a
    def op(f: A => A, g: A => A): (A => A) = {
      f compose g
    }
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(f(a), b))
  }

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val ff = (f.curried)
    val g: B => B = foldMap(as, endoMonoid[B])(ff)
    g(z)
  }

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val m: Monoid[B => B] = dual(endoMonoid[B])
    val ff: A => (B => B) = a => b => f(b, a)

    val g: B => B = foldMap(as, m)(ff)
    g(z)

  }

  /**
   * 采用递归方式实现的
   */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  import ch8.MyGen
  def monoidLaws[A](m: Monoid[A], gen: MyGen[A]) = {
    import ch8.Prop
    import ch8.MyGenX._
    // Associativity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  }

  /**
   * 是否是 排序好的队列
   * 1 最笨的办法， 重新排序， 在比较是否相等
   * 2 比较 每个元素是否比 后面的元素大／小
   * 3
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mgt = new Monoid[Option[(Int, Int, Boolean)]] {
      val zero = None
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        println(o1, o2)
        val res = (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some(x1 min x2, y1 max y2, p && q && x1 >= y2)
          case (x, None) => x
          case (None, x) => x

        }
        println(res)
        res
      }
    }

    val mlt = new Monoid[Option[(Int, Int, Boolean)]] {
      val zero = None
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        println(o1, o2)
        val res = (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some(x1 min x2, y1 max y2, p && q && x1 <= y2)
          case (x, None) => x
          case (None, x) => x

        }
        println(res)
        res
      }
    }

    val bgt: Option[(Int, Int, Boolean)] = foldMapV(ints, mgt)(i => Some((i, i, true)))
    val blt: Option[(Int, Int, Boolean)] = foldMapV(ints, mlt)(i => Some((i, i, true)))
    val orderm = productMonoid(mgt, mlt)
    val order = foldMapV(ints, orderm)(i => (Some((i, i, true)), Some((i, i, true))))
    val (bgt1, blt1) = order

    val r1 = bgt.map(_._3).getOrElse(true) || blt.map(_._3).getOrElse(true)
    val r2 = bgt1.map(_._3).getOrElse(true) || blt1.map(_._3).getOrElse(true)
    println(r1,r2)
    r1
  }

}
package ch11

trait Monad[F[_]] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  // f: (A,B) => C  柯里化  A => B => C 

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]())) { (fa, b) => map2(fa, b)(_ :: _) }

  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]())) { (a, b) => map2(f(a), b)(_ :: _) }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }
  // Recursive version:
  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val res1 = traverse(ms) { a =>
      val r1 = map(f(a)) { b => (a, b) }
      r1
    }
    val res2 = map(res1)(_.filter(_._2).map(_._1))

    res2
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    val ff = (a: Int) => ma
    val gg = f
    val cc = compose(ff, gg)
    cc(1)
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)((fa) => fa)
  }

  def flatMap3[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    val res = map(ma)(f)
    join(res)
  }

  def compose3[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    (a: A) =>
      join(map(f(a))(g))

  }

}

case class Id[A](value: A) {
  def unit[A](a: => A): Id[A] = Id(a)

  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object IdMonad extends Monad[Id] {
  def unit[A](a: => A): Id[A] = {
    Id(a)
  }

  def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = {
    f(ma.value)
  }

}
object Monad {
  import ch8.MyGen
  val genMonad = new Monad[MyGen] {
    def unit[A](a: => A): MyGen[A] = ch8.MyGenX.unit(a)
    def flatMap[A, B](ma: MyGen[A])(f: A => MyGen[B]): MyGen[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap { f }
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap { f }
  }
  def stateMonad[S] = new Monad[({ type f[x] = ch6.State[S, x] })#f] {
    def unit[A](a: => A): ch6.State[S, A] = ch6.State(s => (a, s))
    def flatMap[A, B](st: ch6.State[S, A])(f: A => ch6.State[S, B]): ch6.State[S, B] = {
      st.flatMap { f }
    }
  }
}


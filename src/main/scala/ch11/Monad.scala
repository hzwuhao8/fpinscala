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
}


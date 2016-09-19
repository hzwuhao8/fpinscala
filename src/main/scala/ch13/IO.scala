package ch13

sealed trait IO[A] { self =>
  def run: A
  def ++(io: IO[A]) = new IO[A] {
    def run = {
      self.run
      run
    }
  }

  def map[B](f: A => B): IO[B] = {
    new IO[B] {
      def run = f(self.run)
    }
  }

  def flatMap[B](f: A => IO[B]): IO[B] = {
    new IO[B] {
      def run = f(self.run).run
    }
  }
  def as[A, B](a: IO[A])(b: B): IO[B] = map(_ => b)
  def skip[A](a: IO[A]): IO[Unit] = as(a)(())

  def forever[A, B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a.flatMap { _ => t }
  }

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => IO[B]): IO[B] = l match {
    case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
    case _       => IO.unit(z)
  }

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => IO[B]): IO[Unit] = skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => IO[Unit]): IO[Unit] =
    foldM_(l)(())((u, a) => skip(f(a)))

  
}

object IO extends ch11.Monad[IO] {
  def empty: IO[Unit] = new IO[Unit] {
    def run = ()
  }
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = {
    fa.flatMap { f }
  }
  def apply[A](a: => A): IO[A] = unit(a)

  def doWhile[A](a: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
  
  sealed class IORef[A](var value: A) {
    def set(a: A): IO[A] = IO { value = a; a }
    def get: IO[A] = IO { value }
    def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
  }
  
  
}

object Exam {
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine() }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]) {
    converter.run
  }
}


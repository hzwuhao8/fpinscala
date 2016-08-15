package ch6

import State._
case class State[S, +A](run: S => (A, S)) {
 
  def flatMap[S, A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] = {
    State { s =>
      val (a1, s1) = a.run(s)
      f(a1).run(s1)
    }
  }
  def map[S, A, B](a: State[S, A])(f: A => B): State[S, B] = flatMap(a)(a => unit(f(a)))

  def map2[S, A, B, C](a: State[S, A], b: State[S, B])(f: (A, B) => C) = {
    flatMap(a)((a1) => map(b)((b1) => f(a1, b1)))
  }

}
object State {
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val z: State[S, List[A]] = unit(List.empty[A])
    val f2 = (f: State[S, A], acc: State[S, List[A]]) => f.map2(f, acc)(_ :: _)
    fs.foldRight(z)(f2)
  }

  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for {
    s1 <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s1)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
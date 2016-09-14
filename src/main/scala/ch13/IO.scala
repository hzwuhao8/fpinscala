package ch13

trait IO[A] { self =>
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
}

object IO {
  def empty: IO = new IO {
    def run = ()
  }
}
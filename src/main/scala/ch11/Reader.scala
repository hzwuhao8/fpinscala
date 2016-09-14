package ch11

case class Reader[R, A](run: R => A) {

}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = {
      val run = (r: R) => a
      Reader(run)
    }

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      val run = (r: R) => {
        val a = st.run(r)
        val st2 = f(a)
        val b = st2.run(r)
        b
      }
      Reader(run)
    }

  }

  def main(args: Array[String]) {
    val monad = readerMonad[Int]
    val m1 = monad.unit(1)
    val m2 = monad.map(m1)(_.toString)
    
  }
}
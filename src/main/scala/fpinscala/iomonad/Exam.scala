package fpinscala.iomonad

object Exam {

  def main(args: Array[String]) {
    println("IO0")
    IO0.converter //不纯的
    println("IO1")
    IO1.converter.run //纯的， run 在 需要的地方开始执行！
    println("IO1 ECHO")
    IO1.echo.run

    val p = IO1.IO.forever(IO1.PrintLine("Still going..."))
    p.run
  }

}

object IO2aTests {
  import IO2a._

  /*
  Pg 240: REPL session has a typo, should be:

  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
  }

  Note: we could write a little helper function to make this nicer:

  def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }

  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => suspend { a(x).flatMap(b) }
  }
   */

  val f: Int => IO[Int] = (i: Int) => Return(i)

  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f) {
      (a: Function1[Int, IO[Int]],
      b: Function1[Int, IO[Int]]) =>
        {
          (x: Int) => IO.suspend(a(x).flatMap(b))
        }
    }

  def main(args: Array[String]): Unit = {
    println("sleep 30s ")
    Thread.sleep(30 * 1000)
    println("begin")
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}

object IO2bTests {
  import IO2b._
  /**
   * 用不同的f 是否可以   处理 绝大部分的递归调用？
   * 这里的Int 是否可以看做是一个个状态?
   * g 的定义是否可以通用？
   * 看起来有困难。 g 依赖 f
   * f 可以 抽象为
   *  A => TailRec[A]
   *  这样 g 就可以 定义了
   */
  type P = (Int, Int, Int, List[Int])
  type F[A] = Function1[A, TailRec[A]]

  def f(p: P): TailRec[P] = {
    println(p)
    val next = p._1 + p._2
    Return((next, p._1, p._2, next :: p._4))
  }

  def g[A](i: Int, f: F[A]): F[A] =
    List.fill(i)(f).foldLeft(f) {
      (a: F[A], b: F[A]) =>
        {
          (p: A) => TailRec.suspend(a(p).flatMap(b))
        }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(10, f)((1, 0, 0, Nil))
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}


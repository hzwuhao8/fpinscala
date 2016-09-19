package fpinscala.iomonad

object Exam {
  
  
  def main(args: Array[String]){
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
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, IO[Int]],
        b: Function1[Int, IO[Int]]) => {
        (x: Int) => IO.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    println("sleep 30s ")
    Thread.sleep(30*1000)
    println("begin")
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}


object IO2bTests {
  import IO2b._

  val f: ((Int,Int)) => TailRec[(Int,Int)] = p => {
    println(p)
    Return((p._1 +1 ,p._2 * (p._1+1)))
  }

  val g: ((Int,Int)) => TailRec[(Int,Int)] =
    List.fill(10)(f).foldLeft(f){
      (a: Function1[(Int,Int), TailRec[(Int,Int)]],
        b: Function1[(Int,Int), TailRec[(Int,Int)]]) => {
          (p :(Int,Int) ) => TailRec.suspend(a(p).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(1,1)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}


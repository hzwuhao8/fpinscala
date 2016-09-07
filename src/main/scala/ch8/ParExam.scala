package ch8

import ch7.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object ParExam {
  val ES: ExecutorService = Executors.newCachedThreadPool
  def checkUnit() = {
    val prop = MyGenX.forAll(MyGenX.unit(Par.unit(1)))(i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
    Prop.run(prop)
  }

  def main(args: Array[String]): Unit = {
    checkUnit()
  }
}
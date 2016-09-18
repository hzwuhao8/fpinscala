package fpinscala.iomonad

object Exam {
  
  
  def main(args: Array[String]){
    println("IO0")
    IO0.converter //不纯的
    println("IO1")
    IO1.converter.run //纯的， run 在 需要的地方开始执行！
  }
  
}
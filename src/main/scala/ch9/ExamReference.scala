package ch9

object ExamReference {
  val p = Reference
  import p.operators
  
  def main(args: Array[String]) {
    val str = "abcd"
    val p1 = p.string("abc")
    val r1 = p.run(p1)(str)
    println(r1)
    
    val dig = p.regex("[0-9]+".r)
    val hex = p.regex("0[Xx][0-9a-fA-F]+".r)
    
    
    {
      val str="123"
      val r1 = p.run(dig)(str)
      println(r1)
      
      println( p.run(dig)("0xabc"))
      println( p.run(hex)("0xabc"))
    }
    
    {
      val str="s=1+2+3"
      val pc = p.string("s") 
      println(p.run(pc)(str))
      val dig = p.regex("[0-9]+".r)
      println(p.run(dig)("1+2+3"))
      
      val plus = p.string("+")
       println(p.run(plus)("+2+3"))
       val p1 =  dig | plus
       
        println(p.run(p1)("1+2+3"))
    }
    
  }
}
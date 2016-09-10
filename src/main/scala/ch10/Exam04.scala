package ch10

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object Exam04 {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero = Stub("")
    def op(a: WC, b: WC): WC = {
      (a, b) match {
        case (Stub(as), Stub(bs)) => Stub(as + bs)
        case (Stub(as), Part(lStub, words, rStub)) =>
          Part(as + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(bs)) =>
          Part(lStub, words, rStub + bs)
        case (Part(lStuba, wordsa, rStuba), Part(lStubb, wordsb, rStubb)) =>
          val str = rStuba + lStubb
          //str  是完整的还是 有可能也是部分的
          // this is a test??? ; test 后面是否有 ??? 是不知道的，还是 是知道的？
          // 感觉是 ???==""
          val size = if (str == "") 0 else 1

          Part(lStuba, wordsa + size + wordsb, rStubb)
      }
    }
  }

  def count(s: String): Int = {
    def wc(c: Char): WC = {
      if(c.isWhitespace){
        Part("",0,"")
      }else{
        Stub(c.toString)
      }
    }
    
    def unstub(s: String) = s.length() min 1
    
    val b = Monoid.foldMapV(s.toIndexedSeq,wcMonoid){wc }
    val c = b match{
      case Stub(s) => unstub(s)
      case Part(l,w,r) => unstub(l) + w + unstub(r)
    }
    println(c)
    c
  }
  
  def main(args: Array[String]){
    count("this is a test")
    count("this")
    count("")
    count(" ")
    count("1 234")
  }
}
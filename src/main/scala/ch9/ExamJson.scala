package ch9

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
  def apply(t: T) = f(t)
  override def toString() = name

}

object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
  case class Arr(value: Val*) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val
  case object False extends Val {
    def value = false
  }
  case object True extends Val {
    def value = true
  }
  case object Null extends Val {
    def value = null
  }
}

object JSONParse {
  import fastparse.all._

  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space = P(CharsWhile(Whitespace).?)
  val digits = P(CharsWhile(Digits))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)

  val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => Js.Num(x.toDouble))

  val `null` = P("null").map(_ => Js.Null)
  val `false` = P("false").map(_ => Js.False)
  val `true` = P("true").map(_ => Js.True)

  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(StringChars))
  val string =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

  val array =
    P("[" ~/ jsonExpr.rep(sep = ",".~/) ~ space ~ "]").map(Js.Arr(_: _*))

  val pair = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  val obj =
    P("{" ~/ pair.rep(sep = ",".~/) ~ space ~ "}").map(Js.Obj(_: _*))

  val jsonExpr: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space)
}

object ExamJson {
  import fastparse.all.Parsed
  import JSONParse.jsonExpr
  def main(args: Array[String]) {

    val Parsed.Success(value, _) = jsonExpr.parse(
      """{"omg": "123", "wtf": 12.4123}""")
     println(value)
    assert(value == Js.Obj("omg" -> Js.Str("123"), "wtf" -> Js.Num(12.4123)))
  }
}
package ch9

import fastparse.all._

object ExamFastparse {

  def main(args: Array[String]) {
    val parseA = P("a")

    val Parsed.Success(value, successIndex) = parseA.parse("a")
    assert(value == (), successIndex == 1)

    val failure = parseA.parse("bb").asInstanceOf[Parsed.Failure]
    assert(
      failure.lastParser == ("a": P0),

      failure.extra.traced.trace == """parseA:1:1 / "a":1:1 ..."b"""")

    val ab = P("a" ~ "b").!

    val Parsed.Success(x, index) = ab.parse("abc")
    println((x, index))
    val Parsed.Failure(parser, 1, _) = ab.parse("aa")
    assert(parser == ("b": P0))

    val capture1 = P("a".rep.! ~ "b" ~ End)

    val Parsed.Success(xx, ii) = capture1.parse("aaaab")
    println((xx, ii))

    val ab2 = P("a".rep ~ "b")
    val Parsed.Success(_, 8) = ab2.parse("aaaaaaab")
    val Parsed.Success(_, 4) = ab2.parse("aaaba")

    val abc = P("a".rep(sep = "b") ~ "c")
    val Parsed.Success(_, 8) = abc.parse("abababac")
    val Parsed.Failure(parserf, 3, _) = abc.parse("abaabac")

    val ab4 = P("a".rep(min = 2, max = 10, sep = "b")).!

    {
      val Parsed.Success(v, 9) = ab4.parse("ababababaccbaba")
      println(v)
    }

    {
      val Parsed.Success(v, index) = ab4.parse("ababababababababa")
      println(v)
    }

    {
      val ab4c = P("a".rep(min = 2, max = 4, sep = "b") ~ "c")
      val Parsed.Failure(_, 1, _) = ab4c.parse("ac")
      val Parsed.Success(_, 4) = ab4c.parse("abac")
      val Parsed.Success(_, 8) = ab4c.parse("abababac")
      val Parsed.Failure(_, 7, _) = ab4c.parse("abababaac")
    }

    {
      val option = P("c".? ~ "a".rep(sep = "b").! ~ End)

      val Parsed.Success("aba", 3) = option.parse("aba")
      val Parsed.Success("ababa", 6) = option.parse("cababa")
    }

    {
      val either = P("a".rep ~ ("b" | "c" | "d") ~ End)

      val Parsed.Success(_, 6) = either.parse("aaaaab")
      val Parsed.Failure(parser, 5, _) = either.parse("aaaaae")
      assert(parser == ("b" | "c" | "d"))

    }

    {
      val noEnd = P("a".rep ~ "b")
      val withEnd = P("a".rep ~ "b" ~ End)

      val Parsed.Success(_, 4) = noEnd.parse("aaaba")
      val Parsed.Failure(End, 4, _) = withEnd.parse("aaabb")

    }
    {

      val ab = P((("a" | Start) ~ "b").rep ~ End).!

      val Parsed.Success("abab", 4) = ab.parse("abab")
      val Parsed.Success("babab", 5) = ab.parse("babab")

      val Parsed.Failure(parser, 2, _) = ab.parse("abb")

    }
    {
      val ab = P(Start ~ ("a" ~ "b").rep ~ End).!

      val Parsed.Success("abab", 4) = ab.parse("abab")
      val Parsed.Success("ababab", 6) = ab.parse("ababab")

      val Parsed.Failure(parser, 2, _) = ab.parse("abb")
    }
    {
      val Parsed.Success((), 0) = Pass.parse("asdad")
      val Parsed.Failure(Fail, 0, _) = Fail.parse("asdad")
    }
    {
      val finder = P("hay".rep ~ Index ~ "needle" ~ "hay".rep)

      val Parsed.Success(9, _) = finder.parse("hayhayhayneedlehayhayahay")
    }

    {

      val capture1 = P("a".rep.! ~ "b" ~ End)

      val Parsed.Success("aaa", 4) = capture1.parse("aaab")

      val capture2 = P("a".rep.! ~ "b".! ~ End)

      val Parsed.Success(("aaa", "b"), 4) = capture2.parse("aaab")

      val capture3 = P("a".rep.! ~ "b".! ~ "c".! ~ End)

      val Parsed.Success(("aaa", "b", "c"), 5) = capture3.parse("aaabc")

      val captureRep = P("a".!.rep ~ "b" ~ End)

      val Parsed.Success(Seq("a", "a", "a"), 4) = captureRep.parse("aaab")

      val captureOpt = P("a".rep ~ "b".!.? ~ End)

      val Parsed.Success(Some("b"), 4) = captureOpt.parse("aaab")
      val Parsed.Success(Some("b"), 4) = captureOpt.parse("aaab")
    }

    {
      val ab = P("'" ~ AnyChar.! ~ "'")

      val Parsed.Success("-", 3) = ab.parse("'-'")

      val Parsed.Failure(parser, 2, _) = ab.parse("'-='")
      assert(parser == ("'": P0))

    }
    {
      val keyword = P(("hello" ~ &(" ")).!.rep)

      val Parsed.Success(Seq("hello"), _) = keyword.parse("hello hello ")
      val Parsed.Success(Seq(), __) = keyword.parse("helloX")
    }

    {

      val keyword = P("hello" ~ !" " ~ AnyChar ~ "world").!

      val Parsed.Success("hello-world", _) = keyword.parse("hello-world")
      val Parsed.Success("hello_world", _) = keyword.parse("hello_world")
      val Parsed.Success("helloxworld", _) = keyword.parse("helloxworld")
      val Parsed.Success("hellowworld", _) = keyword.parse("hellowworld")

      val Parsed.Failure(parser, 6, _) = keyword.parse("hello world")
      assert(parser == !(" "))

    }

    {
      val binary = P(("0" | "1").rep.!)
      val binaryNum = P(binary.map(Integer.parseInt(_, 2)))

      val Parsed.Success("1100", _) = binary.parse("1100")
      val Parsed.Success(12, _) = binaryNum.parse("1100")

    }

    {
      val s1 = '0' to '9'
      val s2 = 'a' to 'f'
      val s3 = 'A' to 'F'
      val s = s1 ++ s2 ++ s3
      val p1 = P(CharIn(s).rep(1))

      val hex = P("0" ~ ("x" | "X") ~ p1.!)
      val hexNum = P(hex.map(Integer.parseInt(_, 16)))
      val Parsed.Success("0AcE1", _) = hex.parse("0x0AcE1")
      val Parsed.Success("1100", _) = hex.parse("0x1100")
      val Parsed.Success(0xabcdef, _) = hexNum.parse("0Xabcdef")

    }

    {
      val leftTag = P("<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
      def rightTag(s: String) = P("</" ~ s.! ~ ">")
      val xml = P(leftTag.flatMap(rightTag))

      val Parsed.Failure(x, _, _) = xml.parse("<></>")
      val Parsed.Success("a", _) = xml.parse("<a></a>")
      val Parsed.Success("abcde", _) = xml.parse("<abcde></abcde>")

      val failure = xml.parse("<abcde></edcba>").asInstanceOf[Parsed.Failure]
      assert(
        failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>"""")
    }
    {
      val digits = P(CharIn('0' to '9').rep(1).!).map(_.toInt)
      val even = digits.filter(_ % 2 == 0)
      val Parsed.Success(12, _) = even.parse("12")
      val failure = even.parse("123").asInstanceOf[Parsed.Failure]
      assert(even.toString == "digits.filter(<function1>)")
      assert(failure.extra.traced.trace == "digits.filter(<function1>):1:1 ...\"123\"")
    }

    {
      val digit = CharIn('0' to '9')
      val letter = CharIn('A' to 'Z')
      def twice[T](p: Parser[T]) = p ~ p
      def errorMessage[T](p: Parser[T], str: String) =

        fastparse.core.ParseError(p.parse(str).asInstanceOf[Parsed.Failure]).getMessage

      // Portuguese number plate format since 2006
      val numberPlate = P(twice(digit) ~ "-" ~ twice(letter) ~ "-" ~ twice(digit))

      assert(errorMessage(numberPlate, "11-A1-22") == """
  |found "1-22", expected CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZ") at index 4
  |11-A1-22
  |    ^""".stripMargin.trim)

      // Suppress implementation details from the error message
      val opaqueNumberPlate = numberPlate.opaque("<number-plate>")

      assert(errorMessage(opaqueNumberPlate, "11-A1-22") == """
  |found "11-A1-22", expected <number-plate> at index 0
  |11-A1-22
  |^""".stripMargin.trim)

    }

    {
      val logged = scala.collection.mutable.Buffer.empty[String]
      implicit val logger = fastparse.Logger(logged.append(_))

      val DeepFailure = P("C")
      val Foo = P((DeepFailure.log() | "A".log()) ~ "B".!.log()).log()

      Foo.parse("AB")

      val allLogged = logged.mkString("\n")

      val expected =
        """+Foo:0
    |  +DeepFailure:0
    |  -DeepFailure:0:Failure(DeepFailure:1:1 / "C":1:1 ..."AB")
    |  +"A":0
    |  -"A":0:Success(1)
    |  +"B":1
    |  -"B":1:Success(2)
    |-Foo:0:Success(2)
    |
  """.stripMargin.trim
      assert(allLogged == expected)
    }

    {
      val cp = P(CharPred(_.isUpper).rep.! ~ "." ~ End)

      val Parsed.Success("ABC", _) = cp.parse("ABC.")
      val Parsed.Failure(_, 2, _) = cp.parse("ABc.")

    }
    {
      val ci = P(CharIn("abc", "xyz").rep.! ~ End)

      val Parsed.Success("aababbccxyz", _) = ci.parse("aababbccxyz")
      val Parsed.Failure(_, 7, _) = ci.parse("aaabbccdxyz.")

      val digits = P(CharIn('0' to '9').rep.!)

      val Parsed.Success("12345", _) = digits.parse("12345abcde")
      val Parsed.Success("123", _) = digits.parse("123abcde45")
    }

    {
      val cw = P(CharsWhile(_ != ' ').!)

      val Parsed.Success("12345", _) = cw.parse("12345")
      val Parsed.Success("123", _) = cw.parse("123 45")

    }

    {
      val si = P(StringIn("cow", "cattle").!.rep)

      val Parsed.Success(Seq("cow", "cattle"), _) = si.parse("cowcattle")
      val Parsed.Success(Seq("cow"), _) = si.parse("cowmoo")
    }

    {
      val alpha = P(CharIn('a' to 'z'))
      val nocut = P("val " ~ alpha.rep(1).! | "def " ~ alpha.rep(1).!)

      val Parsed.Success("abcd", _) = nocut.parse("val abcd")

      val failure = nocut.parse("val 1234").asInstanceOf[Parsed.Failure]
      assert(
        failure.index == 0,
        failure.extra.traced.trace ==
          """nocut:1:1 / ("val " ~ alpha.rep(1) | "def " ~ alpha.rep(1)):1:1 ..."val 1234"""")
    }
    {
      val alpha = P(CharIn('a' to 'z'))
      val nocut = P("val " ~/ alpha.rep(1).! | "def " ~/ alpha.rep(1).!)
      val Parsed.Success("abcd", _) = nocut.parse("val abcd")

      val failure = nocut.parse("val 1234").asInstanceOf[Parsed.Failure]
      println(failure.index)
      println(failure.extra.traced.trace)
      assert(
        failure.index == 4,
        failure.extra.traced.trace ==
          """nocut:1:1 / alpha:1:5 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:5 ..."1234""""")

    }

    {
      val alpha = P(CharIn('a' to 'z'))
      val stmt = P("val " ~/ alpha.rep(1).! ~ ";" ~ " ".rep)
      val stmts = P(stmt.rep(1) ~ End)

      val Parsed.Success(Seq("abcd"), _) = stmts.parse("val abcd;")
      val Parsed.Success(Seq("abcd", "efg"), _) = stmts.parse("val abcd; val efg;")

      val failure = stmts.parse("val abcd; val ").asInstanceOf[Parsed.Failure]
      assert(
        failure.index == 14,
        failure.extra.traced.trace ==
          """stmts:1:1 / stmt:1:11 / alpha:1:14 / CharIn("abcdefghijklmnopqrstuvwxyz"):1:14 ...""""")

    }

    {
      val digits = P(CharIn('0' to '9').rep(1))
      val tuple = P("(" ~ digits.!.rep(sep = "," ~/ Pass) ~ ")")

      val Parsed.Success(Seq("1", "23"), _) = tuple.parse("(1,23)")

      val failure = tuple.parse("(1,)").asInstanceOf[Parsed.Failure]
      assert(
        failure.index == 3,
        failure.extra.traced.trace == """tuple:1:1 / digits:1:4 / CharIn("0123456789"):1:4 ...")"""")

    }

    {
      val digit = P(CharIn('0' to '9'))
      val time1 = P(("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm"))
      val time2 = P((("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit)
      val Parsed.Success((), _) = time1.parse("12:30pm")
      val Parsed.Success((), _) = time2.parse("17:45")
      val time = P(time1 | time2)
      val Parsed.Success((), _) = time.parse("12:30pm")
      val failure = time.parse("17:45").asInstanceOf[Parsed.Failure]
      assert(failure.index == 5) // Expects am or pm
    }

    {
      val digit = P(CharIn('0' to '9'))
      val time1 = P(("1".? ~ digit) ~ ":" ~/ digit ~ digit ~ ("am" | "pm"))
      val time2 = P((("1" | "2").? ~ digit) ~ ":" ~/ digit ~ digit)
      val Parsed.Success((), _) = time1.parse("12:30pm")
      val Parsed.Success((), _) = time2.parse("17:45")
      val time = P(NoCut(time1) | time2)
      val Parsed.Success((), _) = time.parse("12:30pm")
      val Parsed.Success((), _) = time.parse("17:45")
    }
  }

}
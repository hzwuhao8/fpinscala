package ch8

case class Sample(i: Int, c: Char, s: String)

object ExamSGen {
  val rng = ch6.SimpleRNG(System.currentTimeMillis())
  def checkSample() = {
    val iGen = MyGenX.choose(0, 100)
    val cGen = MyGenX.atoZ
    val sGen = MyGenX.string()
    val sampleGen: MyGen[Sample] = for {
      i <- iGen
      c <- cGen
      s <- sGen
    } yield {
      Sample(i, c, s)
    }

    val prop = MyGenX.forAll(sampleGen) { sample =>
      println(sample)
      sample.i < 100
    }
    Prop.run(prop)
  }

  def checkmax() = {
    val smallGen = MyGenX.string()
    val sGen = MyGenX.listOf(smallGen)
    val maxProp = MyGenX.forAll(sGen) { ns =>
      println(ns)
      if (ns.isEmpty) {
        true
      } else {
        val max = ns.max
        ns.forall(_ <= max)
      }
    }

    Prop.run(maxProp)

  }
  def checkreverse() = {
    val smallGen = MyGenX.string()
    val reverseProp = MyGenX.forAll(smallGen) { str =>
      println(str)
      str.reverse.reverse == str
    }
    Prop.run(reverseProp)
  }

  def checkSorted() = {
    val smallGen = MyGenX.choose(0, 100)
    val list = MyGenX.listOf(smallGen)
    val prop = MyGenX.forAll(list) { ns =>
      println(ns)
      ns.sorted == ns.reverse.sorted
    }
    Prop.run(prop)
  }

  def main(args: Array[String]): Unit = {

    //checkSample()
    checkSorted()
  }
}
